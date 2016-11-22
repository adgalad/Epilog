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
  , newTemp
  , (#)
  , addTAC
  , comment
  , terminate
  , closeModule
  , enterScope
  , exitScope
  , getVarName
  , newVar
  , insertVar'
  , insertVar
  -- * State
  , dataSegment
  , modules
  , blocks
  , edges
  , currentBlock
  , nextBlock
  , retLabel
  , labelCount
  , tempCount
  , varSupply
  , global
  , symbols
  , varTable
  -- , types
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC      hiding (modules)
import           Language.Epilog.SymbolTable (Scope, SymbolTable)
--------------------------------------------------------------------------------
import           Control.Lens                (at, makeLenses, use, (%%=), (%=),
                                              (&), (.=), (<<+=), (?=), (?~), _2,
                                              _Just, _head)
import           Control.Monad.Trans.State   (StateT, evalStateT, execStateT,
                                              runStateT)
import           Data.Graph                  (Edge, buildG)
import qualified Data.Map                    as Map (empty, lookup)
import           Data.Maybe                  (fromMaybe)
import           Data.Sequence               as Seq (empty)
--------------------------------------------------------------------------------

type IRMonad a = StateT IRState IO a

data IRState = IRState
  { _dataSegment  :: Seq Data
  , _modules      :: Seq Module
  , _blocks       :: Seq (Label, Block)
  , _edges        :: [Edge]
  , _currentBlock :: Maybe (Label, Seq TAC)
  , _nextBlock    :: [Label]
  , _retLabel     :: Maybe Label
  , _labelCount   :: Int
  , _tempCount    :: Int
  , _varSupply    :: Map String Int
  , _global       :: Scope
  , _symbols      :: SymbolTable
  , _varTable     :: [Map String String] }

initialIR :: IRState
initialIR = IRState
  { _dataSegment  = Seq.empty
  , _modules      = Seq.empty
  , _blocks       = Seq.empty
  , _edges        = []
  , _currentBlock = Nothing
  , _nextBlock    = []
  , _retLabel     = Nothing
  , _labelCount   = 1
  , _tempCount    = 0
  , _varSupply    = Map.empty
  , _global       = undefined
  , _symbols      = undefined
  , _varTable     = [] }

makeLenses ''IRState

runIR :: (a -> IRMonad b) -> a -> IO (b, IRState)
runIR x inp = runStateT (x inp) initialIR

newLabel :: IRMonad Label
newLabel = labelCount <<+= 1

newTemp :: IRMonad Operand
newTemp = T <$> (tempCount <<+= 1)

(#) :: Label -> IRMonad ()
(#) label = -- do
  use currentBlock >>= \case
    Nothing -> currentBlock .= Just (label, Seq.empty)
    Just cb -> internal $ "unterminated block \n" <> show cb

addTAC :: TAC -> IRMonad ()
addTAC tac = -- do
  currentBlock %= \case
    Nothing     -> internal "attempted to add instruction without a block"
    cb@(Just _) -> (_Just . _2 |>~ tac) cb

comment :: String -> IRMonad ()
comment = addTAC . Comment

terminate :: Terminator -> IRMonad ()
terminate term = -- do
  use currentBlock >>= \case
    Nothing        -> internal $ "attempted to terminate without a block\n" <> emit term
    Just (lbl, tacs) -> do
      blocks |>= (lbl,) Block
        { lbl
        , tacs
        , term }
      currentBlock .= Nothing

      edges %= (fmap (lbl,) (targets term) <>)

closeModule :: Name -> IRMonad ()
closeModule mName = do
  use currentBlock >>= \case
    Just _ -> internal $
      "attempted to close a module with pending block\n" <> mName
    Nothing -> do
      mBlocks <- fmap snd <$> use blocks
      mEdges <- use edges
      modules |>= Module
        { mName
        , mBlocks
        , mGraph = buildG (-1, length mBlocks) mEdges }

      blocks      .= Seq.empty
      edges       .= []
      nextBlock   .= []
      symbols     .= undefined

enterScope :: IRMonad ()
enterScope = varTable %= (Map.empty :)

exitScope :: IRMonad ()
exitScope = varTable %= tail

getVarName :: String -> IRMonad String
getVarName v = do
  getVarName' <$> use varTable
  where
    getVarName' []     = error "getVarName failed"
    getVarName' (m:ms) = fromMaybe (getVarName' ms) (v `Map.lookup` m)

newVar :: String -> IRMonad String
newVar name = varSupply %%= \supply -> case name `Map.lookup` supply of
  Nothing -> (name <> ".0"         , supply & at name ?~ 1)
  Just i  -> (name <> "." <> show i, supply & at name ?~ i + 1)

insertVar' :: String -> IRMonad ()
insertVar' = void . insertVar

insertVar :: String -> IRMonad String
insertVar v = do
  v' <- newVar v
  varTable._head.at v ?= v'
  pure v'
