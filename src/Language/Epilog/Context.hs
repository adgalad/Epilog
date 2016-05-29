{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( ContextError (..)
    , ContextState (..)
    , ProcSignature (..)
    , Errors
    , Strings
    , Types
    , Context
    , context
    , verifyExpr
    , inst
    , def
    , initialState
    , binaryOperation
    , literal
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
import qualified Control.Monad.Identity          as Id          
import           Data.Function                   (on)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Sequence                   (Seq, (><), (|>), (<|), ViewL((:<)))
import qualified Data.Sequence                   as Seq
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Name         = String
type Strings      = Map String (Seq Position)
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
        { utTName :: Name
        , utVar   :: Name
        , utP     :: Position
        }
    | UndefinedProcedure
        { upName    :: Name
        , upP       :: Position
        }
    | InvalidMember
        { imName :: Name
        , imP    :: Position
        }
    | MemberOfArray
        { moaName :: Name
        , moaP    :: Position
        }
    | InvalidIndex
        { iiP :: Position }
    | NoMain
        { nmP :: Position }
    deriving (Eq)

err :: a -> RWS r (Seq a) s () 
err = tell . Seq.singleton

instance P ContextError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        OutOfScope                 _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        UndefinedType            _ _ p -> p
        UndefinedProcedure         _ p -> p
        InvalidMember              _ p -> p
        MemberOfArray              _ p -> p
        InvalidIndex                 p -> p
        NoMain                       p -> p

instance Ord ContextError where
    compare = compare `on` pos

instance Show ContextError where
    show = \case
        DuplicateDefinition name fstP sndP ->
            "Duplicate definition at " ++ showP sndP ++ ": `" ++ name ++
            "` already defined at " ++ showP fstP

        OutOfScope name p ->
            "Out of scope variable at " ++ showP p ++ ": `" ++ name ++
            "` not available in this scope."

        DuplicateDeclaration name fstT fstP sndT sndP ->
            "Duplicate declaration at " ++ showP sndP ++ ", variable `" ++
            name ++ "` already defined as `" ++ show fstT ++ "` at " ++
            showP fstP ++ " cannot be redeclared as `" ++ show sndT ++ "`"

        UndefinedType nameT var p ->
            "Attempted to declare variable of undefined type at " ++ showP p ++
            ", type `" ++ nameT ++ "`, variable `" ++ var ++ "`"

        UndefinedProcedure  name p ->
            "Call to undeclared procedure at " ++ showP p ++ " named `" ++
            name ++ "`"

        InvalidMember member p ->
            "Invalid member requested at " ++ showP p ++ " named `" ++
            member ++ "`"

        MemberOfArray member p ->
            "Expected array index instead of member at " ++ showP p ++
            " member named `" ++ member ++ "`"

        InvalidIndex p ->
            "Index of non-array variable at " ++ showP p

        NoMain _ ->
            "No procedure `main` defined in the program"

-- State Type --------------------------
data ContextState = ContextState
    { symbols   :: SymbolTable
    , strings   :: Strings
    , pendProcs :: Pending
    , procs     :: Procs
    , types     :: Types
    , expr      :: Seq Expression
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
    -- , ("ref??",     (voidT  , ST.empty, Epilog)) -- This will change
    , ("string",    (stringT, ST.empty, Epilog))
    , ("void",      (voidT  , ST.empty, Epilog))
    ] -- Must be ascending

initialState :: ContextState
initialState  = ContextState
    { symbols      = ST.empty
    , strings      = Map.empty
    , pendProcs    = Map.empty
    , procs        = Map.fromAscList languageProcs
    , types        = Map.fromAscList basicTypes
    , expr         = Seq.empty
    }

-- The Monad ---------------------------
type Context = RWS () Errors ContextState

-- Context -----------------------------
context :: Program -> Int
context str = 1;
--context String = (symbols, strings, types, procs, errors)
--    where
--        (ContextState {symbols, strings, pendProcs, procs, types}, e) =
--            execRWS (mapM_ def decs) () initialState

--        errors' =
--            Seq.sort $ e >< Map.foldrWithKey pendToErrors Seq.empty pendProcs

--        errors = if "main" `Map.member` procs
--            then errors'
--            else NoMain Code <| errors'

--        pendToErrors name ps errs =
--            errs >< foldr (pendToError name) Seq.empty ps

--        pendToError name p errs =
--            errs |> UndefinedProcedure  name p


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
verifyExpr (LitString p string ) = modify (\s ->
    s { strings = Map.insertWith (><) string (Seq.singleton p) (strings s) })
verifyExpr (ECall     p n exprs) = do
    verifyCall n p
    sequence_ $ fmap verifyExpr exprs
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
            if size type0 /= 0
                then do
                    err $ MemberOfArray member p
                    return Nothing
                else
                    case typeName type0 `Map.lookup` ts of
                        Nothing -> return Nothing
                        Just (_, st, _) -> case member `ST.lookup` st of
                            Left _ -> do
                                err $ InvalidMember member p
                                return Nothing
                            Right Entry { varType } -> return (Just varType)
verifyLval (Index lval expr) p = do
    t <- verifyLval lval p
    verifyExpr expr
    case t of
        Nothing -> return Nothing
        Just type0@(Type name dims) ->
            if size type0 == 0
                then err (InvalidIndex p) >> return Nothing
                else return $ Just (Type name (Seq.drop 1 dims))

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
declaration entry@(Entry name t val p) = do
    case val of
        Nothing  -> return ()
        Just expr -> verifyExpr expr

    ContextState { symbols, types } <- get
    case name `local` symbols of
        Right Entry {varType, varPosition} ->
            err $ DuplicateDeclaration name varType varPosition t p
        Left _ -> case typeName t `Map.lookup` types of
            Just _  -> modify (\s -> s
                { symbols = insertSymbol name entry symbols })
            Nothing ->
                err $ UndefinedType (typeName t) name p

openScope' :: Position -> Context ()
openScope' p = modify (\s -> s {symbols = openScope p (symbols s) })

closeScope' :: Context ()
closeScope' = modify (\s -> s { symbols = case goUp (symbols s) of
                                            Left _ -> symbols s
                                            Right x -> x })
binaryOperation :: BinaryOp -> Context ()
binaryOperation op = do
    exp <- gets expr
    case Seq.viewl exp of
        e2 :< xs -> case Seq.viewl xs of 
            e1 :< xs ->
                modify (\s -> s {expr = xs |> Binary (pos e1) op e1 e2 })



literal :: Expression -> Context ()
literal lit = do 
    verifyExpr (lit)
    modify (\s-> s {expr = lit <| (expr s) }) 