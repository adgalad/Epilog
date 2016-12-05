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
  , runStateT
  , execStateT
  , evalStateT
  , runMIPS
  , initialMIPS
  , addMIPS
  , registers
  , variables
  , instructions
  , blocks
  , resetRegDescrips
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

data VarLoc 
  = Global { name   :: String }
  | Local  { offset :: Int32 }
  | None

data RegDescrip = RegDescrip
    { values     :: [IR.Operand]
    , genPurpose :: Bool
    , dirty      :: Bool
    }

instance Ord RegDescrip where
  compare r1 r2 = 
    compare (length . values $ r1) (length . values $ r2)

data VarDescrip = VarDescrip
    { reg      :: Maybe Register
    , taint    :: Bool 
    , location :: VarLoc
  }

data MIPSState = MIPSState
  { _registers    :: Map Register RegDescrip 
  , _variables    :: Map IR.Operand VarDescrip 
  , _instructions :: Seq MIPS
  , _blocks       :: Seq Block }

regs :: [Register]    
regs = (A <$> [1..3]) <> (T <$> [0..9]) <> (S <$> [0..7]) -- Only general use Regs

initialMIPS :: MIPSState
initialMIPS = MIPSState
  { _registers    = Map.fromList [(x,RegDescrip [] True False) | x <- regs]
  , _variables    = Map.empty
  , _instructions = Seq.empty
  , _blocks       = Seq.empty }

makeLenses ''MIPSState

resetRegDescrips :: MIPSMonad ()
resetRegDescrips = do
  registers .= Map.fromList [(x,RegDescrip [] True False) | x <- regs]
  pure ()

addMIPS :: MIPS -> MIPSMonad ()
addMIPS = (instructions |>=)

runMIPS :: (a -> MIPSMonad b) -> a -> IO (b, MIPSState)
runMIPS x inp = runStateT (x inp) initialMIPS

spill :: Register
spill = undefined

getOpReg :: Operand -> Register
getOpReg = undefined

getReg :: IR.TAC -> (Register, Register, Register)
getReg = undefined



