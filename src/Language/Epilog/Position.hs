{-# LANGUAGE LambdaCase #-}

module Language.Epilog.Position
    ( Position (..)
    , P
    , col
    , pos
    , row
    , showP
    ) where
--------------------------------------------------------------------------------

newtype Position = Position (Int, Int)
    deriving (Bounded, Eq, Ord)

instance Show Position where
    show = \case
        Position (0, 0) -> "in epilog"
        Position (r, 0) -> "row " ++ show r
        Position (r, c) -> "row " ++ show r ++ ", col " ++ show c

showP :: Position -> String
showP (Position p) = show p

row, col :: Position -> Int
row (Position (r, _)) = r
col (Position (_, c)) = c

class P a where
    pos :: a -> Position
