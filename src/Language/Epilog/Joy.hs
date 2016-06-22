module Language.Epilog.Joy
    ( Joy (..)
    , joy
    , noJoy
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression  (Exps, Lval)
import           Language.Epilog.AST.Instruction (Insts)
import           Language.Epilog.Position        (Position (..))
import           Language.Epilog.Type            (Type (..))
--------------------------------------------------------------------------------
import           Data.Sequence                   as Seq (empty)
--------------------------------------------------------------------------------

-- | Data type for passing values in the parser
data Joy = Joy
    { jPos    :: Position
    , jType   :: Type
    , jInsts  :: Insts
    , jExps   :: Exps
    , jLval   :: Maybe Lval
    }

joy :: Joy
joy =  Joy
    { jPos    = Code
    , jType   = EpVoid
    , jInsts  = Seq.empty
    , jExps   = Seq.empty
    , jLval   = Nothing
    }

noJoy :: Joy
noJoy = joy { jType = None }
