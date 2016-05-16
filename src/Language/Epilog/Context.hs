{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( context
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction hiding (Set)
import qualified Language.Epilog.AST.Instruction as Inst (Set)
import           Language.Epilog.AST.Type
import           Language.Epilog.AST.Program
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable     hiding (empty)
import qualified Language.Epilog.SymbolTable     as ST (empty)
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict  (RWS, evalRWS, execRWS, get,
                                                  gets, modify, tell)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
--------------------------------------------------------------------------------

type Name    = String
type Strings = Set String
type Types   = Map String (Type, Position)
type Pending = Map String Entry
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
    deriving (Eq)

err :: a -> RWS r (Seq a) s ()
err = tell . Seq.singleton

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

context :: Program -> IO Errors
context (Program decs) = do
    let e = snd $ evalRWS (mapM_ def decs) () initialState

    --Printing Symbol Table and Types
    let t = fst $ execRWS (mapM_ def decs) () initialState
    print $ types t
    putStr . drawTree . toTree . defocus $ symbols t

    return e

-- Definitions --------------------------------------------------------
def :: Definition -> Context ()
def GlobalD { gPos, gType, gName, gVal} =
    verifyDeclaration (EntryVar gName gType gVal gPos)

def StructD { sPos, sName {-, sClass, sConts -} } = do
    t <- gets types
    case sName `Map.lookup` t of
        Just (_, p) -> err $ DuplicateDefinition sName p sPos
        Nothing     -> modify (\s -> s
            { types = Map.insert sName (userT sName, sPos) (types s)
            , pending = Map.delete sName (pending s)
            })

def ProcD { pPos, pName, pParams, pType, pInsts } = do
    let entry = EntryProc pName pType pPos
    modify (\s -> s {symbols = insertSymbol pName entry (symbols s)})

    openScope' pPos
    sequence_ $ fmap param pParams

    openScope' pPos
    sequence_ $ fmap inst pInsts

    closeScope'
    closeScope'


-- Parameters --------------------------------------------------------
param :: Parameter -> Context ()
param (Parameter p t name) =
    verifyDeclaration (EntryVar name t Nothing p)


-- Instructions --------------------------------------------------------
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

-- Expression --------------------------------------------------------

verifyExpr :: Expression -> Context ()
verifyExpr (VarId        p name) = verifySymbol name p
verifyExpr (Binary    _ _ e0 e1) = verifyExpr e0 >> verifyExpr e1
verifyExpr (Unary     _ _ e0   ) = verifyExpr e0
verifyExpr (LitString  _ string) = modify (\s -> s
    { strings = Set.insert string (strings s)})
verifyExpr                     _ = modify id


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
                , pending = Map.insert name entry pending
                })


openScope' :: Position -> Context ()
openScope' p = modify (\s -> s {symbols = openScope p (symbols s) })


closeScope' :: Context ()
closeScope' = modify (\s -> s { symbols = case goUp (symbols s) of
                                            Left _ -> symbols s
                                            Right x -> x })
