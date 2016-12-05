{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Epilog.MIPS.MIPS
  ( MIPS (..)
  , Emips (..)
  , BOp (..)
  , Register (..)
  , Constant (..)
  , Program (..)
  , Block (..)
  , TAC.Label
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC (BOp(..), Data(..))
import qualified Language.Epilog.IR.TAC as TAC
--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Data.Sequence          ((|>))
import           GHC.Generics           (Generic)
import           Safe                   (atDef)
--------------------------------------------------------------------------------

class Emips a where
  emips :: a -> String


instance Emips TAC.Label where
  emips = ("_" <>) . TAC.lblstr

--------------------------------------------------------------------------------

data Register -- = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  = Zero
  | Scratch Word
  | General Word
  | GP
  | SP
  | FP
  deriving (Eq, Show, Read, Ord)

instance Emips Register where
  emips Zero = "$0"
  emips (Scratch n) = atDef
    (internal $ "can't scratch" <> show n)
    ["$v0", "$v1", "$a0", "$ra"]
    n
  emips (General n) = atDef
    (internal $ "no general use register " <> show n)
    [ "$a1", "$a2", "$a3", "$t0", "$t1", "$t2", "$t3"
    , "$t4", "$t5", "$t6", "$t7", "$s0", "$s1", "$s2"
    , "$s3", "$s4", "$s5", "$s6", "$s7", "$t8", "$t9" ]
    n
  emips GP = "$gp"
  emips SP = "$sp"
  emips FP = "$fp"

--------------------------------------------------------------------------------

data Constant
  = IC Word32
  | FC Float
  | CC Word8
  deriving (Eq, Show, Read)

instance Emips Constant where
  emips = \case
    IC i -> show i
    FC f -> show f
    CC w -> show w

--------------------------------------------------------------------------------

instance (Emips a, Foldable f) => Emips (f a) where
  emips = concat . fmap emips . toList



--------------------------------------------------------------------------------
instance Emips TAC.Data where
  emips = \case
    VarData { dName, dSpace } ->
      dName <> ": .space " <> show dSpace <> "\n"
    StringData { dName, dString } ->
      dName <> ": .ascii" <> show dString <> "\n"

--------------------------------------------------------------------------------

data Block = Block
  { bLabel :: TAC.Label
  , bCode  :: Seq MIPS }

instance Emips Block where
  emips Block { bLabel, bCode } =
    (<> "\n") . unlines . ((emips bLabel <> ":") :) . toList $
      fmap (("\t" <>) . emips) bCode

data Program = Program
  { pData   :: Seq TAC.Data
  , pBlocks :: Seq Block
  }

instance Emips Program where
  emips Program { pData, pBlocks } =
    (if null pData then "" else "\t.data\n" <> emips pData <> "\n") <>
    "\t.text\n" <> emips pBlocks
--------------------------------------------------------------------------------
data MIPS
  = Comment String
  | BinOp   BOp Register Register Register
  | BinOpi  BOp Register Register Constant
  | LoadI   Register (Int32)
  | LoadW   Register (Int32, Register)
  | Move    Register Register
  | StoreW  Register (Int32, Register)
  | Syscall
  | Slt  Register Register Register
  -- Terminators
  | Beq  Register Register TAC.Label
  | Bne  Register Register TAC.Label
  | Bc1t TAC.Label
  | Bc1f TAC.Label
  | J    TAC.Label
  | Jr   Register
  | Jal  String

  deriving (Eq, Show, Read)


instance Emips MIPS where
  emips (Comment str) = "# " <> str
  emips (BinOp  op r1 r2 r3) = case op of
    DivI -> emips op <> intercalate ", " (emips <$> [r2,r3]) <> "\n\t" <>
            -- Handle divide by zero exception
            "mflo " <> emips r1
    RemI -> emips op <> intercalate ", " (emips <$> [r2,r3]) <> "\n\t" <>
            -- Handle divide by zero exception
            "mfhi " <> emips r1
    _    -> emips op <> intercalate ", " (emips <$> [r1,r2,r3])

  emips (BinOpi op r1 r2 c ) = case op of
    DivI -> emips op <>  ", " <> emips r2 <> ", " <> emips c <> "\n\t" <>
            -- Handle divide by zero exception
            "mflo " <> emips r1
    RemI -> emips op <>  ", " <> emips r2 <> ", " <> emips c <> "\n\t" <>
            -- Handle divide by zero exception
            "mfhi " <> emips r1
    _    -> emips op <> intercalate ", " (emips <$> [r1,r2]) <> ", " <> emips c

  emips (LoadI r1 i)    = "li " <> emips r1 <> ", " <> show i

  emips (LoadW r1 (c,r2))    =
    "lw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

  emips (Move r1 r2) =  "mv " <> intercalate ", " (emips <$> [r1,r2])

  emips (StoreW r1 (c,r2))    =
    "sw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

  emips Syscall = "syscall"

  emips (Slt r1 r2 r3) = "slt " <> intercalate ", " (emips <$> [r1,r2,r3])

  emips t = case t of
    Beq  r1 r2 label ->
      "beq " <> intercalate ", " (emips <$> [r1,r2]) <> ", " <> emips label

    Bne  r1 r2 label ->
      "bne " <> intercalate ", " (emips <$> [r1,r2]) <> ", " <> emips label

    Bc1t r  -> "bc2t " <> emips r
    Bc1f r  -> "bc2f " <> emips r
    J label -> "j "    <> emips label
    Jr r    -> "jr "   <> emips r
    Jal str -> "jal "  <> str

instance Emips BOp where
  emips = (<> " ") . \case
    AddI -> "add"
    SubI -> "sub"
    MulI -> "mult"
    DivI -> "div"
    RemI -> "div"
    AddF -> "add.s"
    SubF -> "sub.s"
    MulF -> "mul.s"
    DivF -> "div.s"
    BSL  -> "sll"
    BSR  -> "sra"
    BAnd -> "and"
    BOr  -> "or"
    BXor -> "xor"
