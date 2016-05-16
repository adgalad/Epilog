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

data Position = Position (Int, Int) | Epilog
    deriving (Eq, Ord)

instance Show Position where
    show = \case
        Position (r, 0) -> "row " ++ show r
        Position (r, c) -> "row " ++ show r ++ ", col " ++ show c
        Epilog          -> "in epilog"

showP :: Position -> String
showP (Position p) = show p
showP Epilog       = "(in epilog)"

row, col :: Position -> Int
row (Position (r, _)) = r
row Epilog            = 0
col (Position (_, c)) = c
col Epilog            = 0

class P a where
    pos :: a -> Position
