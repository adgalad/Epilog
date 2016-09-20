{-# LANGUAGE TemplateHaskell #-}

module Language.Epilog.IR.Monad
  ( IRState (..)
  , blocks, currentBlock, labelCount, registerSupply
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC
--------------------------------------------------------------------------------
import           Control.Lens           (makeLenses, (<<+=))
import           Data.Graph             (Graph, Table, Vertex)
import qualified Data.Map               as Map (empty)
--------------------------------------------------------------------------------

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

newLabel :: IR Int
newLabel = labelCount <<+= 1
