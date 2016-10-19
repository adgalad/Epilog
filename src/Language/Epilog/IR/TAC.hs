{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Language.Epilog.IR.TAC
  ( TAC (..)
  , Emit (..)
  , Terminator (..)
  , Operation (..)
  , BOp (..)
  , UOp (..)
  , Rel (..)
  , Block (..)
  , Module (..)
  , Data (..)
  , Program (..)
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
import           Data.Graph             (Graph)
import           Data.Serialize         (Serialize)
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------

class Emit a where
  emit :: a -> String

type Label = Int
--------------------------------------------------------------------------------

data Operand
  = R Name
  | T Int
  | C Constant
  | FP
  -- | GP
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Operand where
  emit = \case
    R s -> s
    T i -> "_t" <> show i
    C c -> "#" <> emit c
    FP  -> "@FramePointer"
    -- GP  -> "@GlobalPointer"

data Constant
  = BC Bool
  | IC Int32
  | FC Float
  | CC Word8
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Constant where
  emit = \case
    BC b -> show b
    IC i -> "i" <> show i
    FC f -> "f" <> show f
    CC w -> "c" <> show w
--------------------------------------------------------------------------------

data Block = Block
  { lbl  :: Label
  , tacs :: Seq TAC
  , term :: Terminator }
  deriving (Read, Show, Generic, Serialize)

instance Emit Block where
  emit Block { lbl, tacs, term } =
    (<> "\n") . unlines . ((show lbl <> ":") :) . toList $
      fmap (("\t" <>) . emit) tacs |> (("\t" <>) . emit) term
--------------------------------------------------------------------------------

data Module = Module
  { mName   :: Name
  , mBlocks :: Seq Block
  , mGraph  :: Graph }
  deriving (Read, Show, Generic, Serialize)

instance Emit Module where
  emit Module { mName, mBlocks {-, mGraph-} } =
    "; Module " <> mName <> "\n" <> emit mBlocks
--------------------------------------------------------------------------------

data Data
  = VarData
    { dName  :: Name
    , dSpace :: Int32 }
  | StringData
    { dName   :: Name
    , dString :: String }
  deriving (Read, Show, Generic, Serialize)

instance Emit Data where
  emit = \case
    VarData { dName, dSpace } -> dName <> ": space " <> show dSpace
    StringData { dName, dString } ->
      dName <> ": " <> show dString
--------------------------------------------------------------------------------

data Program = Program
  { datas   :: Seq Data
  , modules :: Seq Module }

instance Emit Program where
  emit Program { datas, modules } =
    (if null datas then "" else ("\t.data\n" <> emit datas <> "\n\n")) <>
    "\t.text\n" <> emit modules
--------------------------------------------------------------------------------

instance (Emit a, Foldable f) => Emit (f a) where
  emit = concat . fmap emit . toList
--------------------------------------------------------------------------------

type Function = String

data TAC
  = Comment String
  -- ^ We're gonna need this

  | Operand :=  Operation
  -- ^ Regular Op-assignment
  | Operand :=# (Int32, Operand)
  -- ^ Array reading, i.e. a := const[i]
  | (Int32, Operand) :#= Operand
  -- ^ Array write, i.e. const[i] := a
  | Operand :=* Operand
  -- ^ Pointer read, i.e. a := *b
  | Operand :*= Operand
  -- ^ Pointer write, i.e. *a := b

  | Param Operand
  -- ^ For storing procedure parameters
  | Call Function
    -- ^ Call a procedure
  | Operand :<- Function
  -- ^ Call a procedure, assign the result to the operand
  | Cleanup Word32
  -- ^ Cleanup the stack (n bytes) after a function call
  | Prelude Word32
  -- ^ Reserve space (n bytes) in the stack
  | Epilog Word32
  -- ^ Free space (n bytes) in the stack
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

infix 8 :=, :=#, :#=, :=*, :*=

instance Emit TAC where
  emit = \case
    Comment s     -> "; " <> s
    x := op       -> emit x <> " := " <> emit op
    x :=# (c, i)  -> emit x <> " := " <> show c <> "[" <> emit i <> "]"
    (c, i) :#= x  -> show c <> "[" <> emit i <> "]" <> " := " <> emit x
    x :=* a       -> emit x <> " := *" <> emit a
    x :*= a       -> "*" <> emit x <> " := " <> emit a
    Param op      -> "param " <> emit op
    Call func     -> "call " <> show func
    x :<- func    -> emit x <> " := call " <> show func
    Cleanup i     -> "cleanup " <> show i
    Prelude i     -> "prelude " <> show i
    Epilog i      -> "epilog "  <> show i

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
  = NegF | NegI | BNot
  deriving (Eq, Show, Ord, Read, Generic, Serialize)


data Terminator
  = Br
    { dest :: Label}
  | IfBr
    { cond      :: Operand
    , trueDest  :: Label
    , falseDest :: Label }
  | CondBr
    { rel       :: Rel
    , op0       :: Operand
    , op1       :: Operand
    , trueDest  :: Label
    , falseDest :: Label }
  | Return
    { retVal :: Maybe Operand }
  | Exit
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Terminator where
  emit = \case
    Br l                 -> "goto " <> show l
    IfBr cond l1 l2      -> "if " <> emit cond <> " goto " <> show l1 <>
      " else goto " <> show l2
    CondBr rel a b l1 l2 ->
      "if " <> fmap toLower (show rel) <> " " <> emit a <> " " <> emit b <>
      " goto " <> show l1 <> "\n\tgoto " <> show l2
    Return op            -> "return" <> maybe "" ((" " <>) . emit) op
    Exit                 -> "exit"

data Rel
  = LTF | LEF
  | GTF | GEF
  | EQF | NEF

  | LTI | LEI
  | GTI | GEI
  | EQI | NEI
  | FAI | NFI
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

targets :: Terminator -> [Label]
targets = \case
  Br { dest } -> [dest]
  IfBr { trueDest, falseDest } -> [trueDest, falseDest]
  CondBr { trueDest, falseDest } -> [trueDest, falseDest]
  Return {} -> []
  Exit -> [-1]
