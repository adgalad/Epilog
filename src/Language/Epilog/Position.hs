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

data Position = Position (Int, Int) | Epilog | Code
    deriving (Eq, Ord)

instance Show Position where
    show = \case
        Position (r, 0) -> "in row " ++ show r
        Position (r, c) -> "at row " ++ show r ++ ", col " ++ show c
        Epilog          -> "in epilog"
        Code            -> "in the program code"

showP :: Position -> String
showP (Position p) = show p
showP Epilog       = "(epilog)"
showP Code         = "(the program code)"

row, col :: Position -> Int
row (Position (r, _)) = r
row Epilog            = 0
row Code              = 0
col (Position (_, c)) = c
col Epilog            = 0
col Code              = 0

class P a where
    pos :: a -> Position
