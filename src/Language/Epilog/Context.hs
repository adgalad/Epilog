{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( ContextError (..)
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
import qualified Language.Epilog.SymbolTable     as ST (empty)
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict  (RWS, execRWS, get, gets,
                                                  modify, tell)
import           Data.Function                   (on)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Sequence                   (Seq, (><), (<|))
import qualified Data.Sequence                   as Seq
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
--------------------------------------------------------------------------------

type Name    = String
type Strings = Set String
type Types   = Map String (Type, Position)
type Pending = Map String (Seq Entry)
type Errors  = Seq ContextError

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
    | UndeclaredType
        { uType    :: String
        , uEntries :: Entry
        }
    deriving (Eq)

err :: a -> RWS r (Seq a) s ()
err = tell . Seq.singleton

instance P ContextError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        OutOfScope                 _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p

instance Ord ContextError where
    compare = compare `on` pos

instance Show ContextError where
    show = \case
        (DuplicateDefinition name fstP sndP) ->
            "Duplicate definition at " ++ showP sndP ++ ", `" ++ name ++
            "` already defined at " ++ showP fstP
        (OutOfScope name p) ->
            "Variable `" ++ name ++ "` out of scope at " ++ showP p
        (DuplicateDeclaration name fstT fstP sndT sndP) ->
            "Duplicate declaration at " ++ showP sndP ++ ", variable `" ++
            name ++ "` already defined as `" ++ show fstT ++ "` at " ++
            showP fstP ++ " cannot be redeclared as `" ++ show sndT
        (UndeclaredType t (EntryVar name _ _ p)) ->
            "Variable `" ++ name ++ "` at " ++ showP p ++
            " declared with an unknown type `" ++ t ++"`"

data ContextState = ContextState
    { symbols :: SymbolTable
    , strings :: Strings
    , pending :: Pending
    , types   :: Types
    }

languageProc :: [(String, Entry)]
languageProc =
    [ ("read",        EntryProc "read"        voidT  Epilog)
    , ("write",       EntryProc "write"       voidT  Epilog)
    , ("toBoolean",   EntryProc "toBoolean"   boolT  Epilog)
    , ("toCharacter", EntryProc "toCharacter" charT  Epilog)
    , ("toFloat",     EntryProc "toFloat"     floatT Epilog)
    , ("toInteger",   EntryProc "toInteger"   intT   Epilog)
    ]

basicTypes :: [(String, (Type, Position))]
basicTypes =
    [ ("character", (charT  , Epilog))
    , ("float",     (floatT , Epilog))
    , ("integer",   (intT   , Epilog))
    , ("ref??",     (voidT  , Epilog)) -- This will change
    , ("string",    (stringT, Epilog))
    , ("void",      (voidT  , Epilog))
    ] -- Must be ascending

initialState :: ContextState
initialState  = ContextState
    { symbols = foldr (uncurry insertSymbol) ST.empty languageProc
    , strings = Set.empty
    , pending = Map.empty
    , types   = Map.fromAscList basicTypes
    }

type Context = RWS () Errors ContextState

context :: Program -> (SymbolTable, Strings, Types, Errors)
context (Program decs) = (symbols, strings, types, errors)
    where
        (ContextState {symbols, strings, pending, types}, e) =
            execRWS (mapM_ def decs) () initialState
        errors = foldr (flip (><).pendToError)
                       Seq.empty (Map.toAscList pending) >< e
        pendToError (t,entries)
            = foldr ((<|).UndeclaredType t) Seq.empty entries


-- Definitions --------------------------------------------------------
def :: Definition -> Context ()
def GlobalD { gPos, gType, gName, gVal} =
    verifyDeclaration (EntryVar gName gType gVal gPos)

def StructD { sPos, sName , sClass, sConts } = do
    t <- gets types
    case sName `Map.lookup` t of
        Just (_, p) -> err $ DuplicateDefinition sName p sPos
        Nothing     -> modify (\s -> s
            { types   = Map.insert sName (userT sName, sPos) (types s)
            , pending = Map.delete sName (pending s)
            })
    sequence_ $ fmap content sConts

def ProcD { pPos, pName, pParams, pType, pInsts } = do
    let entry = EntryProc pName pType pPos
    modify (\s -> s {symbols = insertSymbol pName entry (symbols s)})

    openScope' pPos
    sequence_ $ fmap param pParams

    openScope' pPos
    sequence_ $ fmap inst pInsts

    closeScope'
    closeScope'


-- Parameters and Content -----------------------------------------------------
param :: Parameter -> Context ()
param (Parameter p t name) =
    verifyDeclaration (EntryVar name t Nothing p)

content :: Content -> Context ()
content (Content p t name) =
    verifyDeclaration (EntryVar name t Nothing p)


-- Instructions ---------------------------------------------------------------
inst :: Instruction -> Context ()
inst (Declaration p t name val) =
    verifyDeclaration (EntryVar name t val p)

inst (Assign p (Variable name) rval) = do
    verifySymbol name p
    verifyExpr rval

inst (Call  p name exprs) = do
    verifySymbol name p
    sequence_ $ fmap verifyExpr exprs

inst (If _ guards) =
    sequence_ $ fmap guard guards

inst (Case _ expr sets) = do
    verifyExpr expr
    sequence_ $ fmap set sets

inst (For p var ranges) = do
    verifySymbol var p
    sequence_ $ fmap range ranges

inst (ForD p (Declaration _ t name _) ranges) = do
    openScope' p
    verifyDeclaration (EntryVar name t Nothing p)
    sequence_ $ fmap range ranges
    closeScope'

inst (While _ guards) =
    sequence_ $ fmap guard guards

inst (Read  p (Variable name)) =
    verifySymbol name p

inst (Write _ expr) =
    verifyExpr expr

inst (Finish _) = modify id

guard :: (Position, Expression, Insts) -> Context ()
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

-- Expression -----------------------------------------------------------------

verifyExpr :: Expression -> Context ()
verifyExpr (Lval         p lval) = verifyLval lval p
verifyExpr (Binary    _ _ e0 e1) = verifyExpr e0 >> verifyExpr e1
verifyExpr (Unary     _ _ e0   ) = verifyExpr e0
verifyExpr (LitString  _ string) = modify (\s -> s
    { strings = Set.insert string (strings s)})
verifyExpr                     _ = modify id


verifyLval :: Lval -> Position -> Context ()
verifyLval = undefined


verifySymbol :: String -> Position -> Context ()
verifySymbol name p = do
    s <- gets symbols
    if isSymbol name s
        then modify id
        else err $ OutOfScope name p


verifyDeclaration :: Entry -> Context ()
verifyDeclaration entry@(EntryVar name t _ p) = do
    ContextState { symbols, pending, types } <- get
    case local name symbols of
        Right EntryVar {varType, varPosition} ->
            err $ DuplicateDeclaration name varType varPosition t p
        Left _ -> case typeName t `Map.lookup` types of
            Just _  -> modify (\s -> s
                { symbols = insertSymbol name entry symbols })
            Nothing -> modify (\s-> s
                { symbols = insertSymbol name entry symbols
                , pending =
                    Map.insertWith (flip (><)) (typeName t) (Seq.singleton entry) pending
                })


openScope' :: Position -> Context ()
openScope' p = modify (\s -> s {symbols = openScope p (symbols s) })


closeScope' :: Context ()
closeScope' = modify (\s -> s { symbols = case goUp (symbols s) of
                                            Left _ -> symbols s
                                            Right x -> x })
