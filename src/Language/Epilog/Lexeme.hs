module Language.Epilog.Lexeme
    ( Lexeme(..)

    , module Language.Epilog.Position
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Classes
import           Language.Epilog.Position
--------------------------------------------------------------------------------

data Lexeme a = Lexeme
    { position :: Position
    , token    :: a
    } deriving (Eq, Ord, Show, Read)

instance NiceShow a => NiceShow (Lexeme a) where
    niceShow (Lexeme p a) = niceShow a ++ niceShow p ++ "\n"
