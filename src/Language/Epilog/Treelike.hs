module Language.Epilog.Treelike
    ( Treelike
    ) where
--------------------------------------------------------------------------------
import           Data.Tree (Tree, Forest)
--------------------------------------------------------------------------------

class Treelike a where
    toTree   :: a -> Tree String
    toForest :: [a] -> Forest String
