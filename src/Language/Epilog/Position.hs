module Language.Epilog.Position
    ( Position(..)
    , row
    , col
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Classes
--------------------------------------------------------------------------------

newtype Position = Position (Int, Int)
  deriving (Bounded, Eq, Ord, Show, Read)

instance NiceShow Position where
    niceShow (Position tuple) = case tuple of
        (r,0) -> "POSITION: row " ++ show r
        (r,c) -> "POSITION: row " ++ show r ++ ", col " ++ show c

row, col :: Position -> Int
row (Position (r, _)) = r
col (Position (_, c)) = c
