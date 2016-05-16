{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( context
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction hiding (Set)
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
        { ddName    :: Name
        , ddFirstP  :: Position
        , ddSecondP :: Position
        }
    deriving (Eq)

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
def (GlobalD p (Declaration _ t name val)) =
    verifyDeclaration (EntryVar name t val p)

def StructD {sPosition, sName, sClass, sContents} = do
    t <- gets types
    case sName `Map.lookup` t of
        Just (_, p) -> err $ DuplicateDefinition sName p sPosition
        Nothing     -> modify (\s -> s { types = Map.insert sName (userT sName, sPosition) (types s)
                                       , pending = Map.delete sName (pending s)
                                       })

def (ProcD p name parameters t insts) = do
    modify (\s -> s {symbols = insertSymbol name entry (symbols s)})
    openScope' p
    sequence_ $ fmap param parameters
    openScope' p
    sequence_ $ fmap inst insts

    -- How to know when a scope ends (?)
    closeScope'
    closeScope'

    where
        entry = EntryProc name t  p

-- Parameters --------------------------------------------------------
param :: Parameter -> Context ()
param (Parameter p t name) =
    verifyDeclaration (EntryVar name t Nothing p)


-- Instructions --------------------------------------------------------
inst ::Instruction -> Context ()
inst x = case x of

    Declaration p t name val ->
        verifyDeclaration entry
        where
            entry = EntryVar name t val p

    Assign p (Variable name) rval -> do
        s <- gets symbols
        if isSymbol name s
            then modify (\s -> s)
            else tell $ Seq.singleton $ show p ++": Not in scope `"++name++"`"

{-}
| Call        Position Name        Exps

| If          Position Guards
| Case        Position Expression  Sets
| For         Position Name        Ranges
| ForD        Position Instruction Ranges
| While       Position Conds

| Read        Position Lval
| Write       Position Expression

| Finish      Position
-}

-- Expression --------------------------------------------------------

expr :: Expression -> Context ()
expr e = undefined

verifyDeclaration :: Entry -> Context ()
verifyDeclaration entry@(EntryVar name t _ p) = do
    s <- get
    if isLocal name (symbols s)
        then tell $ Seq.singleton $ show p ++": Duplicate Definition of `"++name++"`"
    else if Set.member (typeName t) (types s)
        then modify (\s -> s { symbols = insertSymbol name entry (symbols s) })
    else modify (\s-> s { symbols = insertSymbol name entry (symbols s)
                        , pending = Map.insert name entry (pending s)
                        })

openScope' :: Position -> Context ()
openScope' p = modify (\s -> s {symbols = openScope p (symbols s) })

closeScope' :: Context ()
closeScope' = modify (\s -> s { symbols = case goUp (symbols s) of
                                            Left _ -> symbols s
                                            Right x -> x })
