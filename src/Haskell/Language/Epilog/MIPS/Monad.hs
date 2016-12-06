{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Language.Epilog.MIPS.Monad
  ( MIPSState (..)
  , MIPSMonad
  , RegDesc (..)
  , runMIPS
  , tell
  , tell1
  , registers
  , variables
  , home
  , vsp
  , resetRegDescs
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import qualified Language.Epilog.IR.TAC    as IR
import           Language.Epilog.MIPS.MIPS
--------------------------------------------------------------------------------
import           Control.Lens              ( makeLenses, (.=) )
import           Control.Monad.Trans.RWS   (RWST, evalRWST, execRWST, runRWST,
                                            tell)
import           Data.Array.IO             (IOArray)
import           Data.Array.MArray         (newArray)
import           Data.Graph                (Edge, buildG)
import qualified Data.Map                  as Map (empty)
import qualified Data.Sequence             as Seq (singleton)
--------------------------------------------------------------------------------

type MIPSMonad a = RWST () (Seq MIPS) MIPSState IO a

tell1 :: Monad m => w -> RWST r (Seq w) s m ()
tell1 = tell . Seq.singleton

data RegDesc = RegDesc
  { values :: [IR.Operand]
  , ss     :: Word32
  , dirty  :: Bool }

data MIPSState = MIPSState
  { _registers :: IOArray Word32     RegDesc
  , _variables :: Map     IR.Operand Word32
  , _home      :: Map     IR.Operand Offset 
  , _vsp        :: Int32 }


initialMIPS :: MIPSState
initialMIPS = MIPSState
  { _registers = undefined
  , _variables = Map.empty
  , _home      = Map.empty
  , _vsp        = 0 }

makeLenses ''MIPSState

resetRegDescs :: MIPSMonad ()
resetRegDescs = do
  x <- liftIO $ newArray (0, 20) (RegDesc [] 0 False)
  registers .= x

runMIPS :: (a -> MIPSMonad ()) -> a -> IO (Seq MIPS)
runMIPS x inp = snd <$> execRWST x' () initialMIPS
  where
    x' = resetRegDescs >> x inp
