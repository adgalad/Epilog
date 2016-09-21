{-# LANGUAGE TemplateHaskell #-}

module Language.Epilog.IR.Monad
  ( IRState (..)
  , initialIR
  , newLabel
  , newRegister
  , blocks, currentBlock, labelCount, registerSupply
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC
--------------------------------------------------------------------------------
import           Control.Lens              (at, makeLenses, (%%=), (&), (<<+=),
                                            (?~))
import           Control.Monad.Trans.State
import qualified Data.Map                  as Map (empty, lookup)
--------------------------------------------------------------------------------

type IRMonad a = StateT IRState IO a

data IRState = IRState
  { _blocks         :: Map Int Block
  , _currentBlock   :: Maybe Block
  , _labelCount     :: Int
  , _registerSupply :: Map String Int }

initialIR :: IRState
initialIR = IRState
  { _blocks         = Map.empty
  , _currentBlock   = Nothing
  , _labelCount     = 0
  , _registerSupply = Map.empty }

makeLenses ''IRState

newLabel :: IRMonad Label
newLabel = L <$> (labelCount <<+= 1)

newRegister :: String -> IRMonad Operand
newRegister name = registerSupply %%= \supply -> case name `Map.lookup` supply of
  Nothing -> (R $ name <> ".0"         , supply & at name ?~ 1)
  Just i  -> (R $ name <> "." <> show i, supply & at name ?~ i + 1)
