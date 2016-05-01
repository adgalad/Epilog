{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Program
    ( Program (..)
    , Declaration (..)
    , Declarations
    ) where
--------------------------------------------------------------------------------
import           Data.Sequence                   (Seq)
import           Language.Epilog.AST.Instruction
import           Language.Epilog.At
--------------------------------------------------------------------------------

data Program = Program Declarations deriving (Eq, Show)

type Declarations = Seq (At Declaration)

data Declaration
    = GlobalD
        { globalType  :: At Type
        , globalValue :: Maybe String
        }
    | EitherD
        { eitherName :: At String
        }
    | RecordD
        {
        }
    | ProcD
        { procName  :: At String
        , procVars  :: InstBlock
        , procInsts :: InstBlock
        }
    | FunD
        { funName  :: At String
        , funVars  :: InstBlock
        , funType  :: At Type
        , funInsts :: InstBlock
        }
    deriving (Eq, Show)

-- data Method
--     = Proc (P String) InstBlock InstBlock
--     | Func (P String) InstBlock (P Type) InstBlock
-- instance Show Method where
--     show = \case
--         Proc name param inst -> "Proc " ++ token name ++ " ( "++ showInst ", " param ++ ")\n" ++ showInst "\n" inst
--         Func name param t inst -> "Func " ++ token name ++ " ( "++ showInst ", " param ++ ")" ++ "->" ++ show (token t) ++"\n"++ showInst "\n" inst
