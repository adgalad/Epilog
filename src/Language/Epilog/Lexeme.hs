module Language.Epilog.Lexeme
  ( Lexeme(..)

  , module Language.Epilog.Position
  ) where
--------------------------------------------------------------------------------
import Language.Epilog.Position
--------------------------------------------------------------------------------

data Lexeme a = Lexeme
  { position :: Position
  , token :: a
  } deriving (Eq, Ord)

instance Show a => Show (Lexeme a) where
  show (Lexeme p a) = show p ++ ": " ++ show a
