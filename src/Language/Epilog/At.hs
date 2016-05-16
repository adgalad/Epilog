{-# LANGUAGE DeriveFunctor #-}

module Language.Epilog.At
    ( At (..)
    , Position (..)
    , row
    , col
    , pos
    , item
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Position hiding (col, row)
import qualified Language.Epilog.Position as P (col, row)
--------------------------------------------------------------------------------

data At a = a :@ Position deriving (Bounded, Eq, Ord, Functor)

instance Show a => Show (At a) where
    show (i :@ Position (r, c)) = unlines
        [ show i
        , case c of
            0 -> "POSITION: row " ++ show r
            _ -> "POSITION: row " ++ show r ++ ", col " ++ show c
        ]

instance P (At a) where
    pos (_ :@ p)  = p

row, col :: At a -> Int
row = P.row . pos
col = P.col . pos

item :: At a -> a
item (i :@ _) = i
