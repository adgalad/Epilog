{-# LANGUAGE LambdaCase   #-}

module Language.Epilog.Position
    ( Position (..)
    , P
    , col
    , pos
    , row
    , showP
    ) where
--------------------------------------------------------------------------------

data Position = Position !Int !Int | Epilog | Code | EOFP
    deriving (Eq, Ord)

instance Show Position where
    show = \case
        Position r 0 -> "in row " ++ show r
        Position r c -> "at row " ++ show r ++ ", col " ++ show c
        Epilog       -> "in epilog"
        Code         -> "in the program code"
        EOFP         -> "in the end of file"

showP :: Position -> String
showP (Position p q) = show (p,q)
showP Epilog         = "(epilog)"
showP Code           = "(the program code)"
showP EOFP           = "(EOF)"

row, col :: Position -> Int
row (Position r _) = r
row Epilog         = 0
row Code           = 0
row EOFP           = 0
col (Position _ c) = c
col Epilog         = 0
col Code           = 0
col EOFP           = 0

class P a where
    pos :: a -> Position
