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
  , Label (..)
  , targets
  , divZeroLabel
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

data Label = Label { lblstr :: String, lblnum :: Int  }
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

divZeroLabel :: Label
divZeroLabel = Label 
  { lblstr = "__divZero"
  , lblnum = -1 }

instance Emit Label where
  emit = lblstr
--------------------------------------------------------------------------------

data Operand
  = R Name
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
    (<> "\n") . unlines . ((emit lbl <> ":") :) . toList $
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
    VarData { dName, dSpace } -> dName <> ": space " <> show dSpace <> "\n"
    StringData { dName, dString } ->
      dName <> ": " <> show dString <> "\n"
--------------------------------------------------------------------------------

data Program = Program
  { datas   :: Seq Data
  , modules :: Seq Module }

instance Emit Program where
  emit Program { datas, modules } =
    (if null datas then "" else "\t.data\n" <> emit datas <> "\n") <>
    "\t.text\n" <> emit modules
--------------------------------------------------------------------------------

instance (Emit a, Foldable f) => Emit (f a) where
  emit = concatMap emit . toList
--------------------------------------------------------------------------------

type Function = String

data TAC
  = Comment String
  -- ^ We're gonna need this

  | Var Bool Name Offset Size
  -- ^ Variable allocation

  | Operand :=  Operation
  -- ^ Regular Op-assignment
  | Operand :=# (Operand, Operand)
  -- ^ Array reading, i.e. a := const[i]
  | (Operand, Operand) :#= Operand
  -- ^ Array write, i.e. const[i] := a
  | Operand :=* Operand
  -- ^ Pointer read, i.e. a := *b
  | Operand :*= Operand
  -- ^ Pointer write, i.e. *a := b

  | Operand :=@ (Operand, Operand)
  -- ^ Offset operator, i.e. a := &b + c

  | Operand :=& Operand
  -- ^ Address-of operator, i.e., t := &a + 0

  | Param Operand
  -- ^ For storing procedure parameters
  | RefParam Operand
  -- ^ For storing procedure reference parameters
  | Call Function
    -- ^ Call a procedure
  | Operand :<- Function
  -- ^ Call a procedure, assign the result to the operand
  | Cleanup Word32
  -- ^ Cleanup the stack (n bytes) after a function call
  | Prolog Word32
  -- ^ Reserve space (n bytes) in the stack
  | Epilog Word32
  -- ^ Free space (n bytes) in the stack

  | Answer Operand
  -- ^ Store return value in stack
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

infix 8 :=, :=#, :#=, :=*, :*=

instance Emit TAC where
  emit = \case
    Comment s     -> "; " <> s
    Var r n o s   -> (if r then "ref" else "var") <> " " <> n <> " " <>
      show o <> " " <> show s
    x := op       -> emit x <> " := " <> emit op
    x :=# (b, o)  -> emit x <> " := " <> emit b <> "[" <> emit o <> "]"
    (b, o) :#= x  -> emit b <> "[" <> emit o <> "]" <> " := " <> emit x
    x :=* a       -> emit x <> " := *" <> emit a
    x :*= a       -> "*" <> emit x <> " := " <> emit a
    x :=& a       -> emit x <> " := &" <> emit a
    Param op      -> "param " <> emit op
    RefParam op   -> "param &" <> emit op
    Call func     -> "call " <> show func
    x :<- func    -> emit x <> " := call " <> show func
    Cleanup i     -> "cleanup " <> show i
    Prolog i      -> "prolog " <> show i
    Epilog i      -> "epilog " <> show i
    Answer o      -> "answer " <> emit o

data Operation
  = B  BOp Operand Operand
  | U  UOp Operand
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Operation where
  emit = \case
    B op a b -> emit op <> " " <> emit a <>  " " <> emit b
    U op a   -> emit op <> emit a

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

instance Emit BOp where
  emit = (fmap toLower) . show

data UOp
  = NegF | NegI | BNot | Id
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit UOp where
  emit = \case
    NegF -> "negf "
    NegI -> "negi "
    BNot -> "bnot "
    Id   -> ""


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
  | Exit
  deriving (Eq, Show, Ord, Read, Generic, Serialize)

instance Emit Terminator where
  emit = \case
    Br l                 -> "goto " <> emit l
    IfBr cond l1 l2      -> "if " <> emit cond <> " goto " <> emit l1 <>
      " else goto " <> emit l2
    CondBr rel a b l1 l2 ->
      "if " <> fmap toLower (show rel) <> " " <> emit a <> " " <> emit b <>
      " goto " <> emit l1 <> "\n\tgoto " <> emit l2
    Return               -> "return"
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
  Exit -> [Label "_EXIT" (-1)]
