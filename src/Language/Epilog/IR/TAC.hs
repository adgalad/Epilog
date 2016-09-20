module Language.Epilog.IR.TAC
  ( TAC (..)
  , Operation (..)
  , BOp (..)
  , UOp (..)
  , Rel (..)
  , Block (..)
  , Operand (..)
  , Label (..)
  , Constant (..)
  ) where
--------------------------------------------------------------------------------
import           Data.Word              (Word8)
import           Language.Epilog.Common
--------------------------------------------------------------------------------
data Operand
  = R String
  | C Constant
  deriving (Eq, Show, Ord, Read)

data Constant
  = BC Bool
  | IC Int
  | FC Float
  | CC Word8
  deriving (Eq, Show, Ord, Read)

newtype Label = L Int deriving (Eq, Show, Ord, Read)

data Block = Block
  { tacs :: Seq TAC
  , term :: Terminator }

data TAC
  = Comment String
  -- ^ We're gonna need this
  | Label   Label
  -- ^ All blocks must begin with a label

  | Operand :=  Operation
  -- ^ Regular Op-assignment
  | Operand :=# (Operand, Operand)
  -- ^ Array reading, i.e. a := b[i]
  | (Operand, Operand) :#= Operand
  -- ^ Array write, i.e. a[i] := b
  | Operand :=* Operand
  -- ^ Pointer read, i.e. a := *b
  | Operand :*= Operand
  -- ^ Pointer write, i.e. *a := b

  | Param   Operand
  -- ^ For storing procedure parameters
  | Call    String
  -- ^ Call a procedure
  deriving (Eq, Show, Ord, Read)


data Operation
  = B  BOp Operand Operand
  | U  UOp Operand
  | Id     Operand
  deriving (Eq, Show, Ord, Read)

data BOp
  = AddI | AddF
  | SubI | SubF
  | MulI | MulF
  | DivI | DivF
  | RemI
  | BSL  | BSR
  | BAnd | BOr
  | BXor
  deriving (Eq, Show, Ord, Read)

data UOp
  = UMinus | BNot | Not
  deriving (Eq, Show, Ord, Read)

data Rel
  = LT | LE
  | GT | GE
  | EQ | NE
  deriving (Eq, Show, Ord, Read)

data Terminator
  = Br                           Label
  | CondBr Rel Operand Operand Label Label
  | BrIfZ  Operand              Label Label
  | Return (Maybe Operand)
  | Exit
  deriving (Eq, Show, Ord, Read)
