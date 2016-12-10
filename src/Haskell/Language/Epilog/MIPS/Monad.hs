{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Language.Epilog.MIPS.Monad
  ( MIPSState (..)
  , MIPSMonad
  , RegDesc (..)
  , Dirtyness (..)
  , dirty
  , runMIPS
  , tell
  , tell1
  , registers
  , variables
  , home
  , vsp
  , ssq
  , fstParam
  , resetDescs
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC    (Operand)
import           Language.Epilog.MIPS.MIPS
--------------------------------------------------------------------------------
import           Control.Lens              (makeLenses, use, (.=), (<~))
import           Control.Monad.Trans.RWS   (RWST, evalRWST, tell)
import           Data.Array.IO             (IOArray)
import           Data.Array.MArray         (newArray)
-- import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Map                  as Map (empty)
import qualified Data.Sequence             as Seq (singleton)
--------------------------------------------------------------------------------

type MIPSMonad a = RWST () (Seq MIPS) MIPSState IO a

tell1 :: Monad m => w -> RWST r (Seq w) s m ()
tell1 = tell . Seq.singleton

data Dirtyness = Clean | Dirty deriving (Eq, Show)

data RegDesc = RegDesc
  { values    :: [Operand]
  , ss        :: Word32
  , dirtyness :: Dirtyness }

dirty :: RegDesc -> Bool
dirty = (== Dirty) . dirtyness

data MIPSState = MIPSState
  { _registers :: IOArray Word32  RegDesc
  , _variables :: Map     Operand Register
  , _home      :: Map     Operand Offset
  , _vsp       :: Int32
  , _ssq       :: [Int32] -- Stack Size Queue
  , _fstParam  :: Bool }


initialMIPS :: MIPSState
initialMIPS = MIPSState
  { _registers = undefined
  , _variables = Map.empty
  , _home      = Map.empty
  , _vsp       = 0
  , _ssq       = []
  , _fstParam  = False}

makeLenses ''MIPSState

resetDescs :: MIPSMonad ()
resetDescs = do
  registers  <~ liftIO (newArray (0, 50) (RegDesc [] 0 Clean))
  variables  .= Map.empty

runMIPS :: (a -> MIPSMonad ()) -> a -> IO ([Int32], Seq MIPS)
runMIPS x inp = evalRWST x' () initialMIPS
  where
    x' :: MIPSMonad [Int32]
    x' = resetDescs >> x inp >> fmap (tail . reverse) (use ssq)
