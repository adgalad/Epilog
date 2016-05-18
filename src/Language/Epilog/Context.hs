{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( ContextError (..)
    , ProcSignature (..)
    , Errors
    , Strings
    , Types
    , context
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction hiding (Set)
import qualified Language.Epilog.AST.Instruction as Inst (Set)
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Type
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable     hiding (empty)
import qualified Language.Epilog.SymbolTable     as ST (empty, lookup)
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict  (RWS, execRWS, get, gets,
                                                  modify, tell)
import           Control.Monad                   (foldM, void, unless)
import           Data.Function                   (on)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Sequence                   (Seq, (><), (|>))
import qualified Data.Sequence                   as Seq
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Name         = String
type Strings      = Set String
type Types        = Map Name (Type, SymbolTable, Position)
type Procs        = Map Name ProcSignature
type Pending      = Map Name (Seq Position)
type Errors       = Seq ContextError

-- Table Element Types -----------------
data ProcSignature = ProcSignature
    { procName     :: Name
    , procType     :: Type
    , procPosition :: Position
    }

-- Error Type --------------------------
data ContextError
    = DuplicateDefinition
        { dDefName :: Name
        , dDefFstP :: Position
        , dDefSndP :: Position
        }
    | OutOfScope
        { oosName :: Name
        , oosP    :: Position
        }
    | DuplicateDeclaration
        { dDecName :: Name
        , dDecFstT :: Type
        , dDecFstP :: Position
        , dDecSndT :: Type
        , dDecSndP :: Position
        }
    | UndefinedType
        { utName :: Name
        , utVar  :: Name
        , utP    :: Position
        }
    | UndeclaredProcedure
        { upName    :: Name
        , upP       :: Position
        }
    | InvalidMember
        { imName :: Name
        , imP    :: Position
        }
    deriving (Eq)

err :: a -> RWS r (Seq a) s ()
err = tell . Seq.singleton

instance P ContextError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        OutOfScope                 _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        UndefinedType            _ _ p -> p
        UndeclaredProcedure        _ p -> p
        InvalidMember              _ p -> p

instance Ord ContextError where
    compare = compare `on` pos

instance Show ContextError where
    show = \case
        DuplicateDefinition name fstP sndP ->
            "Duplicate definition at " ++ showP sndP ++ ", `" ++ name ++
            "` already defined at " ++ showP fstP

        OutOfScope name p ->
            "Variable `" ++ name ++ "` out of scope at " ++ showP p

        DuplicateDeclaration name fstT fstP sndT sndP ->
            "Duplicate declaration at " ++ showP sndP ++ ", variable `" ++
            name ++ "` already defined as `" ++ show fstT ++ "` at " ++
            showP fstP ++ " cannot be redeclared as `" ++ show sndT

        UndefinedType name var p ->
            "Attempted to declare variable of undefined type at " ++ showP p ++
            ", type `" ++ name ++ "`, variable `" ++ var ++ "`"

        UndeclaredProcedure name p ->
            "Call to undeclared procedure at " ++ showP p ++ " named `" ++
            name ++ "`"

        InvalidMember member p ->
            "Invalid member requested at " ++ showP p ++ " named `" ++
            member ++ "`"

-- State Type --------------------------
data ContextState = ContextState
    { symbols   :: SymbolTable
    , strings   :: Strings
    , pendProcs :: Pending
    , procs     :: Procs
    , types     :: Types
    }

languageProcs :: [(Name, ProcSignature)]
languageProcs =
    [ ("toBoolean",   ProcSignature "toBoolean"   boolT  Epilog)
    , ("toCharacter", ProcSignature "toCharacter" charT  Epilog)
    , ("toFloat",     ProcSignature "toFloat"     floatT Epilog)
    , ("toInteger",   ProcSignature "toInteger"   intT   Epilog)
    ] -- Must be ascending

basicTypes :: [(Name, (Type, SymbolTable, Position))]
basicTypes =
    [ ("character", (charT  , ST.empty, Epilog))
    , ("float",     (floatT , ST.empty, Epilog))
    , ("integer",   (intT   , ST.empty, Epilog))
    , ("ref??",     (voidT  , ST.empty, Epilog)) -- This will change
    , ("string",    (stringT, ST.empty, Epilog))
    , ("void",      (voidT  , ST.empty, Epilog))
    ] -- Must be ascending

initialState :: ContextState
initialState  = ContextState
    { symbols      = ST.empty
    , strings      = Set.empty
    , pendProcs    = Map.empty
    , procs        = Map.fromAscList languageProcs
    , types        = Map.fromAscList basicTypes
    }

-- The Monad ---------------------------
type Context = RWS () Errors ContextState

-- Context -----------------------------
context :: Program -> (SymbolTable, Strings, Types, Procs, Errors)
context (Program decs) = (symbols, strings, types, procs, errors)
    where
        (ContextState {symbols, strings, pendProcs, procs, types}, e) =
            execRWS (mapM_ def decs) () initialState

        errors =
            Seq.sort $ e >< Map.foldrWithKey pendToErrors Seq.empty pendProcs

        pendToErrors name ps errs =
            errs >< foldr (pendToError name) Seq.empty ps

        pendToError name p errs =
            errs |> UndeclaredProcedure name p


-- Definitions -------------------------
def :: Definition -> Context ()
def GlobalD { gPos, gType, gName, gVal} =
    declaration (Entry gName gType gVal gPos)

def StructD { sPos, sName, sConts {-, sClass -} } = do
    t <- gets types
    case sName `Map.lookup` t of
        Just (_, _, p) -> err $ DuplicateDefinition sName p sPos
        Nothing -> do
            st <- foldM cont (emptyP sPos) sConts
            modify (\s ->
                s {types = Map.insert sName (userT sName, st, sPos) (types s)})

def ProcD { pPos, pName, pParams, pType, pInsts } = do
    let signature = ProcSignature pName pType pPos
    modify (\s -> s { procs = Map.insert pName signature (procs s) })

    openScope' pPos
    sequence_ $ fmap param pParams

    openScope' pPos
    sequence_ $ fmap inst pInsts

    closeScope'
    closeScope'

-- Contents ----------------------------
cont :: SymbolTable -> Content -> Context SymbolTable
cont st Content { cPos, cType, cName } = do
    ContextState { types } <- get
    case typeName cType `Map.lookup` types of
        Just _ -> case cName `local` st of
            Right Entry { varType, varPosition } -> do
                err $ DuplicateDeclaration cName varType varPosition cType cPos
                return st
            Left _ -> return $ insertSymbol cName entry st
        Nothing -> do
            err $ UndefinedType (typeName cType) cName cPos
            return st
    where
        entry = Entry cName cType Nothing cPos

-- Parameters --------------------------
param :: Parameter -> Context ()
param (Parameter p t name) =
    declaration (Entry name t Nothing p)

-- Instructions ------------------------
inst :: Instruction -> Context ()
inst (Declaration p t name val) =
    declaration (Entry name t val p)

inst (Assign p lval rval) = do
    void $ verifyLval lval p
    verifyExpr rval

inst (ICall p name exprs) = do
    verifyCall name p
    sequence_ $ fmap verifyExpr exprs

inst (If _ guards) =
    sequence_ $ fmap guard guards

inst (Case _ expr sets) = do
    verifyExpr expr
    sequence_ $ fmap set sets

inst (For p Nothing var ranges) = do
    verifySymbol var p
    sequence_ $ fmap range ranges

inst (For p (Just t) var ranges) = do
    openScope' p
    declaration (Entry var t Nothing p)
    sequence_ $ fmap range ranges
    closeScope'

inst (While _ guards) =
    sequence_ $ fmap guard guards

inst (Read p lval) =
    void $ verifyLval lval p

inst (Write _ expr) =
    verifyExpr expr

inst (Finish _) = return ()

guard :: Guard -> Context ()
guard (p, expr, insts) = do
    verifyExpr expr
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

set :: Inst.Set -> Context ()
set (p, exprs, insts) = do
    sequence_ $ fmap verifyExpr exprs
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

range :: Range -> Context ()
range (p, from, to, insts) = do
    verifyExpr from
    verifyExpr to
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

-- Expression --------------------------
verifyExpr :: Expression -> Context ()
verifyExpr (Lval      p lval   ) = void $ verifyLval lval p
verifyExpr (Binary    _ _ e0 e1) = verifyExpr e0 >> verifyExpr e1
verifyExpr (Unary     _ _ e0   ) = verifyExpr e0
verifyExpr (LitString _ string ) = modify (\s ->
    s { strings = Set.insert string (strings s)})
verifyExpr                     _ = return ()

verifyLval :: Lval -> Position -> Context (Maybe Type)
verifyLval (Variable n) p = do
    s <- gets symbols
    case n `ST.lookup` s of
        Right Entry {varType} -> return (Just varType)
        Left _ -> do
            err $ OutOfScope n p
            return Nothing
verifyLval (Member lval member) p = do
    t <- verifyLval lval p
    case t of
        Nothing -> return Nothing
        Just type0 -> do
            ts <- gets types
            case typeName type0 `Map.lookup` ts of
                Nothing -> return Nothing
                Just (_, st, _) -> case member `ST.lookup` st of
                    Left _ -> do
                        err $ InvalidMember member p
                        return Nothing
                    Right Entry { varType } -> return (Just varType)
verifyLval (Index _ _) _ = undefined

verifyCall :: Name -> Position -> Context ()
verifyCall proc p = do
    ContextState { procs, pendProcs } <- get
    unless (proc `Map.member` procs) $
        modify (\ s -> s {pendProcs =
            Map.insertWith (><) proc (Seq.singleton p) pendProcs})

verifySymbol :: String -> Position -> Context ()
verifySymbol name p = do
    s <- gets symbols
    unless (isSymbol name s) $ err $ OutOfScope name p

declaration :: Entry -> Context ()
declaration entry@(Entry name t _ p) = do
    ContextState { symbols, types } <- get
    case name `local` symbols of
        Right Entry {varType, varPosition} ->
            err $ DuplicateDeclaration name varType varPosition t p
        Left _ -> case typeName t `Map.lookup` types of
            Just _  -> modify (\s -> s
                { symbols = insertSymbol name entry symbols })
            Nothing ->
                err $ UndefinedType name (typeName t) p

openScope' :: Position -> Context ()
openScope' p = modify (\s -> s {symbols = openScope p (symbols s) })

closeScope' :: Context ()
closeScope' = modify (\s -> s { symbols = case goUp (symbols s) of
                                            Left _ -> symbols s
                                            Right x -> x })
