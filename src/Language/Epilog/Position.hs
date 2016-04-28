module Language.Epilog.Position
    ( Position(..)
    , row
    , col
    ) where
--------------------------------------------------------------------------------

newtype Position = Position (Int, Int)
    deriving (Bounded, Eq, Ord)

instance Show Position where
    show (Position tuple) = case tuple of
        (r,0) -> "POSITION: row " ++ show r
        (r,c) -> "POSITION: row " ++ show r ++ ", col " ++ show c

row, col :: Position -> Int
row (Position (r, _)) = r
col (Position (_, c)) = c
