module Language.Epilog.TAC
  (
  ) where

newtype Register = Register String deriving (Eq, Show, Ord, Read)
newtype Label    = Label    String deriving (Eq, Show, Ord, Read)
newtype Const    = Const    Int    deriving (Eq, Show, Ord, Read)

data TAC
  = Comment String
  | Label   Label

  | (:=)    Register Operation

  | Br                              Label
  | CondBr  Rel Reference Reference Label
  | BrNZ    Reference               Label
  | BrEZ    Reference               Label
  deriving (Eq, Show, Ord, Read)


data Operation
  = B  BOp Register Register
  | U  UOp Register
  | Id     Register
  deriving (Eq, Show, Ord, Read)

data BOp
  = AddI | AddF
  | SubI | SubF
  | MulI | MulF
  | DivI | DivF

data UOp
  = UMinus | BNot | Not

data Rel
  =
