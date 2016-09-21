module Language.Epilog.Treelike
  ( Tree(..)
  , Treelike
  , drawTree
  , leaf
  , posRoot
  , toForest
  , toForest'
  , toTree
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
--------------------------------------------------------------------------------
import qualified Data.Map.Strict        as Map (toList)
import           Data.Tree              (Forest, Tree (..), drawTree)
--------------------------------------------------------------------------------

class Treelike a where
  toTree   :: a -> Tree String
  toForest :: (Foldable f, Functor f) => f a -> Forest String
  toForest = toList . fmap toTree
  toForest' :: Map String a -> Forest String
  toForest' = fmap (\(a, b) -> Node a [toTree b]) . Map.toList

posRoot  :: (Int, Int) -> Tree String -> Tree String
posRoot pos tree =
  tree {rootLabel = show pos <> rootLabel tree}

leaf :: String -> Tree String
leaf s = Node s []
