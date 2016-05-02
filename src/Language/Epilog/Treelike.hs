module Language.Epilog.Treelike
    ( Treelike
    , toTree
    , toForest
    , posRoot
    , Tree(..)
    , drawTree
    ) where
--------------------------------------------------------------------------------
import           Data.Foldable (toList)
import           Data.Tree     (Forest, Tree (..), drawTree)
--------------------------------------------------------------------------------

class Treelike a where
    toTree   :: a -> Tree String
    toForest :: (Foldable f, Functor f) => f a -> Forest String
    toForest = toList . fmap toTree

posRoot  :: (Int, Int) -> Tree String -> Tree String
posRoot pos tree =
    tree {rootLabel = show pos ++ rootLabel tree}
