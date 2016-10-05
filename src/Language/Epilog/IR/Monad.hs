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
  , runIR
  , initialIR
  , newLabel
  , newRegister
  , newTemp
  , (#)
  , addTAC
  , comment
  , terminate
  -- * State
  , blocks
  , edges
  , currentBlock
  , nextBlock
  , labelCount
  , tempCount
  , registerSupply
  , global
  , symbols
  -- , types
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC
import           Language.Epilog.SymbolTable (Scope, SymbolTable)
--------------------------------------------------------------------------------
import           Control.Lens                (at, makeLenses, use, (%%=), (%=),
                                              (&), (.=), (<<+=), (?~), _2,
                                              _Just)
import           Control.Monad.Trans.State   (StateT, evalStateT, execStateT,
                                              runStateT)
import           Data.Graph                  (Edge)
import qualified Data.Map                    as Map (empty, lookup)
import           Data.Sequence               as Seq (empty)
--------------------------------------------------------------------------------

type IRMonad a = StateT IRState IO a

data IRState = IRState
  { _blocks         :: Seq (Label, Block)
  , _edges          :: [Edge]
  , _currentBlock   :: Maybe (Label, Seq TAC)
  , _nextBlock      :: [Label]
  , _labelCount     :: Int
  , _tempCount      :: Int
  , _registerSupply :: Map String Int
  , _global         :: Scope
  , _symbols        :: SymbolTable }
  -- , _types          :: Types }

initialIR :: IRState
initialIR = IRState
  { _blocks         = Seq.empty
  , _edges          = []
  , _currentBlock   = Nothing
  , _nextBlock      = []
  , _labelCount     = 1
  , _tempCount      = 0
  , _registerSupply = Map.empty
  , _global         = undefined
  , _symbols        = undefined }
  -- , _types          = undefined }

makeLenses ''IRState

runIR :: (a -> IRMonad b) -> a -> IO (b, IRState)
runIR x inp = runStateT (x inp) initialIR

newLabel :: IRMonad Label
newLabel = labelCount <<+= 1

newTemp :: IRMonad Operand
newTemp = T <$> (tempCount <<+= 1)

newRegister :: String -> IRMonad Operand
newRegister name = registerSupply %%= \supply -> case name `Map.lookup` supply of
  Nothing -> (R $ name <> ".0"         , supply & at name ?~ 1)
  Just i  -> (R $ name <> "." <> show i, supply & at name ?~ i + 1)

(#) :: Label -> IRMonad ()
(#) label = -- do
  -- liftIO . putStrLn . (<> ":") . show $ label
  use currentBlock >>= \case
    Nothing -> currentBlock .= Just (label, Seq.empty)
    Just cb -> internal $ "unterminated block \n" <> show cb

addTAC :: TAC -> IRMonad ()
addTAC tac = -- do
  -- liftIO . putStrLn . ("\t" <>) . emit $ tac
  currentBlock %= \case
    Nothing     -> internal "attempted to add instruction without a block"
    cb@(Just _) -> (_Just . _2 |>~ tac) cb

comment :: String -> IRMonad ()
comment = addTAC . Comment

terminate :: Terminator -> IRMonad ()
terminate term = -- do
  -- liftIO . putStrLn . ("\t" <>) . emit $ term
  use currentBlock >>= \case
    Nothing        -> internal $ "attempted to terminate without a block\n" <> emit term
    Just (lbl, tacs) -> do
      blocks |>= (lbl,) Block
        { lbl
        , tacs
        , term }
      currentBlock .= Nothing

      edges %= (fmap (lbl,) (targets term) <>)
