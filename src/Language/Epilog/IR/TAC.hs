{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.IR.TAC
  ( TAC (..)
  , Emit (..)
  , Terminator (..)
  , Operation (..)
  , BOp (..)
  , UOp (..)
  , Rel (..)
  , Block (..)
  , Operand (..)
  , Constant (..)
  , Label
  , targets
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
--------------------------------------------------------------------------------
import           Control.Lens           ((|>))
import           Data.Char              (toLower)
import           Data.Serialize         (Serialize)
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------

type Label = Int
--------------------------------------------------------------------------------

data Operand
  = R String
  | T Int
  | C Constant
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Operand where
  emit = \case
    R s -> s
    T i -> "_t" <> show i
    C c -> "#" <> emit c

data Constant
  = BC Bool
  | IC Int
  | FC Float
  | CC Word8
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Constant where
  emit = \case
    BC b -> show b
    IC i -> "I" <> show i
    FC f -> "F" <> show f
    CC w -> "C" <> show w
--------------------------------------------------------------------------------

class Emit a where
  emit :: a -> String
--------------------------------------------------------------------------------

data Block = Block
  { lbl  :: Label
  , tacs :: Seq TAC
  , term :: Terminator }
  deriving (Read, Show, Generic, Serialize)

instance Emit Block where
  emit Block { lbl, tacs, term } = unlines . ((show lbl <> ":") :) . toList $
    fmap (("\t" <>) . emit) tacs |> (("\t" <>) . emit) term
--------------------------------------------------------------------------------

data TAC
  = Comment String
  -- ^ We're gonna need this

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
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

infix 8 :=, :=#, :#=, :=*, :*=

instance Emit TAC where
  emit = \case
    Comment s    -> ";" <> s
    x := op      -> emit x <> " := " <> emit op
    x :=# (a, i) -> emit x <> " := " <> emit a <> "[" <> emit i <> "]"
    (a, i) :#= x -> emit a <> "[" <> emit i <> "]" <> " := " <> emit x
    x :=* a      -> emit x <> " := *" <> emit a
    x :*= a      -> "*" <> emit x <> " := " <> emit a
    Param op     ->  "param " <> emit op
    Call  f      -> "call" <> f

data Operation
  = B  BOp Operand Operand
  | U  UOp Operand
  | Id     Operand
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Operation where
  emit = \case
    B op a b -> fmap toLower (show op) <> " " <> emit a <>  " " <> emit b
    U op a   -> fmap toLower (show op) <> " " <> emit a
    Id   a   -> emit a

data BOp
  = AddI | AddF
  | SubI | SubF
  | MulI | MulF
  | DivI | DivF
  | RemI
  | BSL  | BSR
  | BAnd | BOr
  | BXor
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

data UOp
  = UMinus | BNot | Not
  deriving (Eq, Show, Ord, Read, Generic, Serialize)


data Terminator
  = Br
    { dest :: Label}
  | CondBr
    { rel       :: Rel
    , op1       :: Operand
    , op2       :: Operand
    , trueDest  :: Label
    , falseDest :: Label }
  | Return
    { retVal :: Maybe Operand }
  | Exit
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Terminator where
  emit = \case
    Br l                 -> "goto " <> show l
    CondBr rel a b l1 l2 ->
      "if " <> fmap toLower (show rel) <> " " <> emit a <> " " <> emit b <>
      " goto " <> show l1 <> " else goto " <> show l2
    Return op            -> "return" <> maybe "" ((" " <>) . emit) op
    Exit                 -> "exit"

data Rel
  = LT | LE
  | GT | GE
  | EQ | NE
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

targets :: Terminator -> [Label]
targets = \case
  Br { dest } -> [dest]
  CondBr { trueDest, falseDest } -> [trueDest, falseDest]
  Return {} -> [] -- FIXME
  Exit -> [-1]
