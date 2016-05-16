{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( context
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.SymbolTable     hiding (empty)
import qualified Language.Epilog.SymbolTable     as ST (empty)
import           Language.Epilog.Treelike
import           Language.Epilog.Position
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Instruction

--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict (RWS, evalRWS, execRWS, modify, gets, tell, get)
import           Data.Sequence                  (Seq, (<|), (><))
import qualified Data.Sequence                  as Seq 
import qualified Data.Set                       as Set 
import           Data.Map                       (Map)
import qualified Data.Map.Strict                as Map 
import           Control.Monad                  (unless, when, sequence_)

--------------------------------------------------------------------------------

type Strings = Set.Set String
type Types   = Set.Set String
type Pending = Map String Entry
type Errors  = Seq String

data ContextState = ContextState
    { symbols :: SymbolTable
    , strings :: Strings
    , pending :: Pending
    , types   :: Types
    }

languageProc :: [(String, Entry)]
languageProc 
    = [ ("read",        EntryProc "read"        (Type "void"      Seq.empty) (Position (1,1)))
      , ("write",       EntryProc "write"       (Type "void"      Seq.empty) (Position (1,1)))
      , ("toBoolean",   EntryProc "toBoolean"   (Type "boolean"   Seq.empty) (Position (1,1)))
      , ("toCharacter", EntryProc "toCharacter" (Type "character" Seq.empty) (Position (1,1)))
      , ("toFloat",     EntryProc "toFloat"     (Type "float"     Seq.empty) (Position (1,1)))
      , ("toInteger",   EntryProc "toInteger"   (Type "integer"   Seq.empty) (Position (1,1)))
      ]

basicTypes :: [String]
basicTypes = [ "integer"
             , "character"
             , "float"
             , "string"
             , "void"
             , "ref??"
             ]

initialState :: ContextState
initialState  = ContextState
    { symbols = foldr (uncurry insertSymbol) (ST.empty) languageProc
    , strings = Set.empty
    , pending = Map.empty
    , types   = Set.fromList basicTypes
    }

type Context = RWS () Errors ContextState

context :: Program -> IO Errors
context (Program decs) = do 
    let e = snd $ evalRWS (mapM_ def decs) () initialState
    
    --Printing Symbol Table and Types
    let t = fst $ execRWS (mapM_ def decs) () initialState
    putStrLn . show $ types t 
    putStr . drawTree . toTree . defocus $ symbols t

    return e

-- Definitions --------------------------------------------------------
def :: Definition -> Context ()
def (GlobalD p (Declaration _ t name val)) =
    verifyDeclaration entry 
  where
    entry = EntryVar name t val p

def (StructD p name _ insts) = do
    t <- gets types
    if Set.member name t 
        then tell $ Seq.singleton $ show p ++": Duplicate Definition of `"++name++"`"
        else modify (\s -> s { types = Set.insert name (types s)
                             , pending = Map.delete name (pending s) 
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
    verifyDeclaration entry 
    where 
        entry = EntryVar name t Nothing p 


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
