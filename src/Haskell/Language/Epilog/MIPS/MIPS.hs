{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Epilog.MIPS.MIPS
  ( MIPS (..)
  , EmitMIPS (..)
  , BOp (..)
  , Register (..)
  , Constant (..)
  , Program (..)
  , Block (..)
  , IR.Label
  -- , v0, v1, a0, a1, a2, a3
  -- , t0, t1, t2, t3, t4, t5, t6, t7, t8, t9
  -- , s0, s1, s2, s3, s4, s5, s6, s7
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC (BOp(..), Data(..))
import qualified Language.Epilog.IR.TAC as IR
--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Data.Sequence          ((|>))
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------

class EmitMIPS a where
  emitMIPS :: a -> String


instance EmitMIPS IR.Label where
  emitMIPS = ("_" <>) . IR.lblstr

--------------------------------------------------------------------------------

data Register = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  deriving (Eq, Show, Read, Ord)

-- v0,v1,a0,a1,a2,a3,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,s0,s1,s2,s3,s4,s5,s6,s7 :: Register
-- [v0,v1]                         = V <$> [0..]
-- [a0,a1,a2,a3]                   = A <$> [0..]
-- [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9] = T <$> [0..]
-- [s0,s1,s2,s3,s4,s5,s6,s7]       = S <$> [0..]

instance EmitMIPS Register where
  emitMIPS Zero  = "$0"
  emitMIPS (V i) = "$v" <> show i
  emitMIPS (A i) = "$a" <> show i
  emitMIPS (T i) = "$t" <> show i
  emitMIPS (S i) = "$s" <> show i
  emitMIPS GP    = "$gp"
  emitMIPS FP    = "$fp"
  emitMIPS SP    = "$sp"
  emitMIPS RA    = "$ra"

--------------------------------------------------------------------------------

data Constant
  = IC Word32
  | FC Float
  | CC Word8
  deriving (Eq, Show, Read)

instance EmitMIPS Constant where
  emitMIPS = \case
    IC i -> show i
    FC f -> show f
    CC w -> show w

--------------------------------------------------------------------------------

instance (EmitMIPS a, Foldable f) => EmitMIPS (f a) where
  emitMIPS = concat . fmap emitMIPS . toList



--------------------------------------------------------------------------------
instance EmitMIPS IR.Data where
  emitMIPS = \case
    VarData { dName, dSpace } -> 
      dName <> ": .space " <> show dSpace <> "\n"
    StringData { dName, dString } ->
      dName <> ": .ascii " <> show dString <> "\n"

--------------------------------------------------------------------------------

data Block = Block 
  { bLabel :: IR.Label
  , bCode  :: Seq MIPS }

instance EmitMIPS Block where
  emitMIPS Block { bLabel, bCode } =
    (<> "\n") . unlines . ((emitMIPS bLabel <> ":") :) . toList $
      fmap (("\t" <>) . emitMIPS) bCode

data Program = Program
  { pData   :: Seq IR.Data
  , pBlocks :: Seq Block
  }

instance EmitMIPS Program where 
  emitMIPS Program { pData, pBlocks } =
    (if null pData then "" else "\t.data\n" <> emitMIPS pData <> "\n") <>
    "\t.text\n" <> emitMIPS pBlocks
--------------------------------------------------------------------------------
data MIPS
  = Comment String
  | BinOp   BOp Register Register Register
  | BinOpi  BOp Register Register Constant
  | LoadI   Register (Int32)
  | LoadW   Register (Int32, Register)
  | Move    Register Register
  | StoreW  Register (Int32, Register)
  | StoreWG Register String
  | Syscall
  | Slt     Register Register Register
  -- Terminators
  | Beq  Register Register IR.Label
  | Bne  Register Register IR.Label
  | Bc1t IR.Label
  | Bc1f IR.Label
  | J    IR.Label
  | Jr   Register
  | Jal  String

  deriving (Eq, Show, Read)


instance EmitMIPS MIPS where
  emitMIPS (Comment str) = "# " <> str
  emitMIPS (BinOp  op r1 r2 r3) = case op of 
    DivI -> emitMIPS op <> intercalate ", " (emitMIPS <$> [r2,r3]) <> "\n\t" <>
            -- Handle divide by zero exception
            "mflo " <> emitMIPS r1 
    RemI -> emitMIPS op <> intercalate ", " (emitMIPS <$> [r2,r3]) <> "\n\t" <>
            -- Handle divide by zero exception
            "mfhi " <> emitMIPS r1 
    _    -> emitMIPS op <> intercalate ", " (emitMIPS <$> [r1,r2,r3])
  
  emitMIPS (BinOpi op r1 r2 c ) = case op of
    DivI -> emitMIPS op <>  ", " <> emitMIPS r2 <> ", " <> emitMIPS c <> "\n\t" <>
            -- Handle divide by zero exception
            "mflo " <> emitMIPS r1 
    RemI -> emitMIPS op <>  ", " <> emitMIPS r2 <> ", " <> emitMIPS c <> "\n\t" <>
            -- Handle divide by zero exception
            "mfhi " <> emitMIPS r1 
    _    -> emitMIPS op <> intercalate ", " (emitMIPS <$> [r1,r2]) <> ", " <> emitMIPS c
  
  emitMIPS (LoadI r1 i)    = "li " <> emitMIPS r1 <> ", " <> show i

  emitMIPS (LoadW r1 (c,r2))    = 
    "lw " <> emitMIPS r1 <> ", " <> show c <> "(" <> emitMIPS r2 <> ")"
  
  emitMIPS (Move r1 r2) =  "mv " <> intercalate ", " (emitMIPS <$> [r1,r2])
  
  emitMIPS (StoreW r1 (c,r2))    = 
    "sw " <> emitMIPS r1 <> ", " <> show c <> "(" <> emitMIPS r2 <> ")"

  emitMIPS (StoreWG r name)    = 
    "sw " <> emitMIPS r <> ", " <> name

  emitMIPS Syscall = "syscall"

  emitMIPS (Slt r1 r2 r3) = "slt " <> intercalate ", " (emitMIPS <$> [r1,r2,r3])

  emitMIPS t = case t of
    Beq  r1 r2 label ->
      "beq " <> intercalate ", " (emitMIPS <$> [r1,r2]) <> ", " <> emitMIPS label
    
    Bne  r1 r2 label ->
      "bne " <> intercalate ", " (emitMIPS <$> [r1,r2]) <> ", " <> emitMIPS label

    Bc1t r  -> "bc2t " <> emitMIPS r
    Bc1f r  -> "bc2f " <> emitMIPS r
    J label -> "j "    <> emitMIPS label   
    Jr r    -> "jr "   <> emitMIPS r  
    Jal str -> "jal "  <> str

instance EmitMIPS BOp where
  emitMIPS = (<> " ") . \case
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



