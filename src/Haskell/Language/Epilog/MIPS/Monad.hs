{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Language.Epilog.MIPS.Monad
  ( MIPSState (..)
  , MIPSMonad
  , runStateT
  , execStateT
  , evalStateT
  , runMIPS
  , initialMIPS

  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import qualified Language.Epilog.IR.TAC      as IR
import           Language.Epilog.MIPS.MIPS
import           Language.Epilog.SymbolTable (Scope, SymbolTable)
--------------------------------------------------------------------------------
import           Control.Lens                (at, makeLenses, use, (%%=), (%=),
                                              (&), (.=), (<<+=), (?~), _2,
                                              _Just)
import           Control.Monad.Trans.State   (StateT, evalStateT, execStateT,
                                              runStateT)
import           Data.Graph                  (Edge, buildG)
import qualified Data.Map                    as Map (empty, lookup, fromList)
import           Data.Array                  (Array, listArray)
import           Data.Sequence               as Seq (empty)
--------------------------------------------------------------------------------

type MIPSMonad a = StateT MIPSState IO a

data VarLocation = Global String | Local Int | Register | None

data MIPSState = MIPSState
  { _registers    :: Map Register [IR.Operand] 
  , _variables    :: Map IR.Operand VarLocation 
  , _instructions :: Seq MIPS }

initialMIPS :: MIPSState
initialMIPS = MIPSState
  { _registers = Map.fromList [(x,[]) | x <- regs]
  , _variables = Map.empty }
  where 
    regs = [Zero,v0,v1,FP,GP,SP,RA] <> (A <$> [0..3]) <> (T <$> [0..9]) <> (S <$> [0..7])

makeLenses ''MIPSState

addInstruction :: MIPS -> MIPSMonad ()
addInstruction = (instructions |>=)

runMIPS :: (a -> MIPSMonad b) -> a -> IO (b, MIPSState)
runMIPS x inp = runStateT (x inp) initialMIPS

getReg :: IR.TAC -> (Int, Int, Int)
getReg = undefined


