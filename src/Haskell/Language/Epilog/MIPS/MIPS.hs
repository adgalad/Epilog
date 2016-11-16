{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Epilog.MIPS.MIPS
  ( MIPS (..)
  , EmitMIPS (..)
  , Terminator (..)
  , BOp (..)
  , Register (..)
  , Constant (..)
  , Label
  , v0, v1, a0, a1, a2, a3
  , t0, t1, t2, t3, t4, t5, t6, t7, t8, t9
  , s0, s1, s2, s3, s4, s5, s6, s7
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC hiding(Terminator,Constant(..), Operand(..))
--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Data.Serialize         (Serialize)
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------

class EmitMIPS a where
  emitMIPS :: a -> String


instance EmitMIPS Label where
  emitMIPS label = "_label" <> show label
--------------------------------------------------------------------------------

data Register = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  deriving (Eq, Show, Read, Ord)

[v0,v1]                         = V <$> [0..]
[a0,a1,a2,a3]                   = A <$> [0..]
[t0,t1,t2,t3,t4,t5,t6,t7,t8,t9] = T <$> [0..]
[s0,s1,s2,s3,s4,s5,s6,s7]       = S <$> [0..]

instance EmitMIPS Register where
  emitMIPS Zero  = "$0"
  emitMIPS (V i) = "$v" <> show i
  emitMIPS (A i) = "$a" <> show i
  emitMIPS (T i) = "$t" <> show i
  emitMIPS (S i) = "$s" <> show i
  emitMIPS GP    = "$gp"
  emitMIPS FP    = "$sp"
  emitMIPS SP    = "$fp"
  emitMIPS RA    = "$ra"

--------------------------------------------------------------------------------

data Constant
  = IC Int32
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
data MIPS
  = BinOp   BOp Register Register Register
  | BinOpi  BOp Register Register Constant
  | LoadW   Register (Int32, Register)
  | StoreW  Register (Int32, Register)
  | Syscall
  deriving (Eq, Show, Read)


instance EmitMIPS MIPS where
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
  
  emitMIPS (LoadW r1 (c,r2))    = 
    "lw " <> emitMIPS r1 <> ", " <> show c <> "(" <> emitMIPS r2 <> ")"
  
  
  emitMIPS (StoreW r1 (c,r2))    = 
    "sw " <> emitMIPS r1 <> ", " <> show c <> "(" <> emitMIPS r2 <> ")"

  emitMIPS Syscall = "syscall"


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

data Terminator 
  = Beq  Register Register Label
  | Bne  Register Register Label
  | Bc1t Label
  | Bc1f Label
  | J    Label
  | Jr   Register
  | Jal 
  deriving (Eq, Show, Read)

instance EmitMIPS Terminator where
  emitMIPS = \case
    Beq  r1 r2 label ->
      "beq " <> intercalate ", " (emitMIPS <$> [r1,r2]) <> ", " <> emitMIPS label
    
    Bne  r1 r2 label ->
      "bne " <> intercalate ", " (emitMIPS <$> [r1,r2]) <> ", " <> emitMIPS label

    Bc1t r  -> "bc2t " <> emitMIPS r
    Bc1f r  -> "bc2f " <> emitMIPS r
    J label -> "j "    <> emitMIPS label   
    Jr r    -> "jr "   <> emitMIPS r  
    Jal     -> "jal"

