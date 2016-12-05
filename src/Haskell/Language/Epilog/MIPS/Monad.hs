{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Language.Epilog.MIPS.Monad
  ( MIPSState (..)
  , MIPSMonad
  , RegDescrip (..)
  , VarDescrip (..)
  , VarLoc (..)
  , runMIPS
  , tell
  , tell1
  , initialMIPS
  , addMIPS
  , registers
  , variables
  , instructions
  , resetRegDescrips
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import qualified Language.Epilog.IR.TAC      as IR
import           Language.Epilog.MIPS.MIPS
--------------------------------------------------------------------------------
import           Control.Lens                (at, makeLenses, use, (%%=), (%=),
                                              (&), (.=), (<<+=), (?~), _2,
                                              _Just)
import           Control.Monad.Trans.RWS     (RWST, evalRWST, execRWST,
                                              runRWST, tell)
import           Data.Graph                  (Edge, buildG)
import qualified Data.Map                    as Map (empty, lookup, fromList)
import           Data.Array                  (Array, listArray)
import qualified Data.Sequence               as Seq (empty, singleton)
--------------------------------------------------------------------------------

type MIPSMonad a = RWST () (Seq MIPS) MIPSState IO a

tell1 :: Monad m => w -> RWST r (Seq w) s m ()
tell1 = tell . Seq.singleton

data VarLoc 
  = Global { name   :: String }
  | Local  { offset :: Int32 }
  | None

data RegDescrip = RegDescrip
  { values     :: [IR.Operand]
  , genPurpose :: Bool
  , dirty      :: Bool }

data VarDescrip = VarDescrip
  { reg      :: Maybe Register
  , taint    :: Bool 
  , location :: VarLoc }

data MIPSState = MIPSState
  { _registers    :: Map Register RegDescrip 
  , _variables    :: Map IR.Operand VarDescrip 
  , _instructions :: Seq MIPS }

regs :: [Register]    
regs = (Scratch <$> [0..20]) -- Only general use Regs

initialMIPS :: MIPSState
initialMIPS = MIPSState
  { _registers    = Map.fromList [(x,RegDescrip [] True False) | x <- regs]
  , _variables    = Map.empty
  , _instructions = Seq.empty }

makeLenses ''MIPSState

resetRegDescrips :: MIPSMonad ()
resetRegDescrips = do
  registers .= Map.fromList [(x,RegDescrip [] True False) | x <- regs]
  pure ()

addMIPS :: MIPS -> MIPSMonad ()
addMIPS = (instructions |>=)

runMIPS :: (a -> MIPSMonad ()) -> a -> IO (Seq MIPS)
runMIPS x inp = snd <$> execRWST (x inp) () initialMIPS

spill :: Register
spill = undefined

getOpReg :: IR.Operand -> Register
getOpReg o = undefined

getReg :: IR.TAC -> (Register, Register, Register)
getReg t = undefined



