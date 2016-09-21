{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

module Language.Epilog.IR.Monad
  ( IRState (..)
  , IRMonad
  , runStateT
  , execStateT
  , evalStateT
  , initialIR
  , newLabel
  , newRegister
  , newTemp
  , (#)
  , addTAC
  , terminate
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC
--------------------------------------------------------------------------------
import           Control.Lens              (at, makeLenses, use, (%%=), (%=),
                                            (&), (.=), (<<+=), (?=), (?~), _2,
                                            _Just)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT,
                                            runStateT)
import           Data.Graph                (Edge)
import qualified Data.Map                  as Map (empty, lookup)
import           Data.Sequence             as Seq (empty)
--------------------------------------------------------------------------------

type IRMonad a = StateT IRState IO a

data IRState = IRState
  { _blocks         :: Map Label Block
  , _edges          :: [Edge]
  , _currentBlock   :: Maybe (Label, Seq TAC)
  , _labelCount     :: Int
  , _tempCount      :: Int
  , _registerSupply :: Map String Int }
  deriving (Show)

initialIR :: IRState
initialIR = IRState
  { _blocks         = Map.empty
  , _edges          = []
  , _currentBlock   = Nothing
  , _labelCount     = 1
  , _tempCount      = 0
  , _registerSupply = Map.empty }

makeLenses ''IRState

newLabel :: IRMonad Label
newLabel = labelCount <<+= 1

newTemp :: IRMonad Operand
newTemp = T <$> (tempCount <<+= 1)

newRegister :: String -> IRMonad Operand
newRegister name = registerSupply %%= \supply -> case name `Map.lookup` supply of
  Nothing -> (R $ name <> ".0"         , supply & at name ?~ 1)
  Just i  -> (R $ name <> "." <> show i, supply & at name ?~ i + 1)

(#) :: Label -> IRMonad ()
(#) label = use currentBlock >>= \case
  Nothing -> currentBlock .= Just (label, Seq.empty)
  Just cb -> internal $ "unterminated block \n" <> show cb

addTAC :: TAC -> IRMonad ()
addTAC tac = currentBlock %= \case
  Nothing     -> internal "attempted to add instruction without a block"
  cb@(Just _) -> (_Just . _2 |>~ tac) cb

terminate :: Terminator -> IRMonad ()
terminate term = use currentBlock >>= \case
  Nothing        -> internal "attempted to terminate without a block"
  Just (lbl, tacs) -> do
    blocks . at lbl ?= Block
      { lbl
      , tacs
      , term }
    currentBlock .= Nothing

    edges %= (fmap (lbl,) (targets term) <>)
