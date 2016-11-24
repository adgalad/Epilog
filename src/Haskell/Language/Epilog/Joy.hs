module Language.Epilog.Joy
  ( Joy (..)
  , joy
  , noJoy
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression  (Expression, Lval)
import           Language.Epilog.AST.Instruction (Guards, IBlock, Insts, Ranges)
import           Language.Epilog.Position        (Position (..))
import           Language.Epilog.Type            (Type (..), voidT)
--------------------------------------------------------------------------------
import           Data.Sequence                   as Seq (empty)
--------------------------------------------------------------------------------

-- | Data type for passing values in the parser
data Joy = Joy
  { jPos    :: Position
  , jType   :: Type
  , jInsts  :: Insts
  , jExp    :: Maybe Expression
  , jExp'   :: Maybe Expression
  , jLval   :: Maybe Lval
  , jRanges :: Ranges
  , jGuards :: Guards
  , jBlock  :: Maybe IBlock }

joy :: Joy
joy = Joy
  { jPos    = Code
  , jType   = voidT
  , jInsts  = Seq.empty
  , jExp    = Nothing
  , jExp'   = Nothing
  , jLval   = Nothing
  , jRanges = Seq.empty
  , jGuards = Seq.empty
  , jBlock  = Nothing }

noJoy :: Joy
noJoy = joy { jType = None }
