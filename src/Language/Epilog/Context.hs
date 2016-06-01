module Language.Epilog.Context
    ( string
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Lexer
--------------------------------------------------------------------------------
import           Control.Lens           ((%=))
import           Data.Sequence          ((><))
import qualified Data.Sequence          as Seq (singleton)
import qualified Data.Map               as Map (insertWith)
--------------------------------------------------------------------------------

string :: At Token -> Epilog ()
string (TokenStringLit s :@ p) = do
    strings %= Map.insertWith (flip (><)) s (Seq.singleton p)
