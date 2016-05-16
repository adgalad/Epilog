{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( context
    ) where
--------------------------------------------------------------------------------
import Language.Epilog.SymbolTable hiding (empty)
import qualified Language.Epilog.SymbolTable as ST (empty)
import Language.Epilog.AST.Program
import Language.Epilog.AST.Instruction
--------------------------------------------------------------------------------
import Control.Monad.Trans.RWS.Strict (RWS, evalRWS, modify)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (empty)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
--------------------------------------------------------------------------------

type Strings = Seq String
type Pending = Map String Entry
type Errors  = Seq String

data ContextState = ContextState
    { symbols :: SymbolTable
    , strings :: Strings
    , pending :: Pending
    }

initialState :: ContextState
initialState  = ContextState
    { symbols = ST.empty
    , strings = Seq.empty
    , pending = Map.empty
    }

type Context = RWS () Errors ContextState

context :: Program -> IO Errors
context (Program decs) = return . snd $ evalRWS (mapM_ def decs) () initialState

def :: Definition -> Context ()
def (GlobalD p (Declaration _ t name val)) =
    modify (\s -> s { symbols = insertSymbol name entry (symbols s)})
  where
    entry = Entry name t val p

def (StructD { }) = modify id
def (ProcD { }) = modify id

