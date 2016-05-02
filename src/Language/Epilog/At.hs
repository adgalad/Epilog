{-# LANGUAGE DeriveFunctor #-}

module Language.Epilog.At
    ( At(..)
    , row
    , col
    , pos
    , item
    ) where
--------------------------------------------------------------------------------
import Language.Epilog.Treelike
--------------------------------------------------------------------------------

data At a = a :@ (Int, Int) deriving (Bounded, Eq, Ord, Functor)

instance Show a => Show (At a) where
    show (i :@ (r, c)) = unlines
        [ show i
        , case c of
            0 -> "POSITION: row " ++ show r
            _ -> "POSITION: row " ++ show r ++ ", col " ++ show c
        ]

row, col :: At a -> Int
row (_ :@ (r, _)) = r
col (_ :@ (_, c)) = c

pos :: At a -> (Int, Int)
pos (_ :@ p)      = p

item :: At a -> a
item (i :@ _)     = i

instance Treelike a => Treelike (At a) where
    toTree (i :@ p) = Node root' children
        where
            Node root children = toTree i
            root' = root ++ " @" ++ show p
