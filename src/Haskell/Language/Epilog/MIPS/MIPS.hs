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
  , Label
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC (BOp (..), Label (..), divZeroLabel, emit)
import qualified Language.Epilog.IR.TAC as IR
--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Safe                   (atDef)
--------------------------------------------------------------------------------

class Emips a where
  emips :: a -> String

instance Emips Label where
  emips = ("_" <>) . IR.lblstr

--------------------------------------------------------------------------------

data Register -- = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  = Zero
  | Scratch Word
  | General Word
  | GP
  | SP
  | FP
  | RA
  deriving (Eq, Show, Read, Ord)

instance Emips Register where
  emips Zero = "$0"
  emips (Scratch n) = atDef
    (internal $ "can't scratch" <> show n)
    ["$v0", "$v1", "$a0"]
    (fromIntegral n)
  emips (General n) = atDef
    (internal $ "no general use register " <> show n)
    [ "$a1", "$a2", "$a3", "$t0", "$t1", "$t2", "$t3"
    , "$t4", "$t5", "$t6", "$t7", "$s0", "$s1", "$s2"
    , "$s3", "$s4", "$s5", "$s6", "$s7", "$t8", "$t9" ]
    (fromIntegral n)
  emips GP = "$gp"
  emips SP = "$sp"
  emips FP = "$fp"
  emips RA = "$ra"

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
data MIPS
  = Comment String
  | MLabel   Label
  -- Sections
  | DataSection
  | TextSection
  -- Declarations
  | Data    Name Int32
  | MString Name String
  -- Instructions
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
  | Beq  Register Register Label
  | Bne  Register Register Label
  | Bc1t Label
  | Bc1f Label
  | J    Label
  | Jr   Register
  | Jal  String
  deriving (Eq, Show, Read)

instance Emips MIPS where
  emips l = tab l <> case l of
    Comment str -> "# " <> str
    MLabel lbl  -> "_" <> emit lbl <> ":"
    Data dName dSpace ->
      dName <> ": .space " <> show dSpace 
    MString dName dString ->
      dName <> ": .asciiz " <> show dString 
    DataSection -> ".data"
    TextSection -> ".text"
    BinOp  op r1 r2 r3 -> case op of
      DivI -> unlines
        [         emips (Beq r3 Zero divZeroLabel)
        , "\t" <> emips DivI <> intercalate ", " (emips <$> [r1, r2,r3]) ]
      RemI -> unlines 
        [         emips (Beq r3 Zero divZeroLabel)
        , "\t" <> emips DivI <> intercalate ", " (emips <$> [r2,r3])
        , "\t" <> "mfhi " <> emips r1 ]
        
      _    -> emips op <> intercalate ", " (emips <$> [r1,r2,r3])

    BinOpi op r1 r2 c -> case op of
      DivI -> emips DivI <> intercalate ", " (fmap emips [r1, r2] <> [emips c])
      RemI -> unlines
        [         emips DivI <> intercalate ", " [emips r2, emips c] 
        , "\t" <> "mfhi " <> emips r1 ]

      _ -> emips op <> intercalate ", " (fmap emips [r1, r2] <> [emips c])

    LoadI r1 i -> "li " <> emips r1 <> ", " <> show i

    LoadW r1 (c,r2) ->
      "lw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

    Move r1 r2 ->  
      "mv " <> intercalate ", " (emips <$> [r1,r2])

    StoreW r1 (c,r2) ->
      "sw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

    Syscall ->
      "syscall"
  
    StoreWG r name ->
      "sw " <> emips r <> ", " <> name

    Slt r1 r2 r3 ->
      "slt " <> intercalate ", " (emips <$> [r1,r2,r3])

    Beq  r1 r2 label ->
      "beq " <> intercalate ", " (emips <$> [r1,r2]) <> ", " <> emips label

    Bne  r1 r2 label ->
      "bne " <> intercalate ", " (emips <$> [r1,r2]) <> ", " <> emips label

    Bc1t r  -> "bc2t " <> emips r
    Bc1f r  -> "bc2f " <> emips r
    J label -> "j "    <> emips label
    Jr r    -> "jr "   <> emips r
    Jal str -> "jal "  <> str

    where
      tab = \case
        Comment {} -> ""
        MLabel  {} -> ""
        Data    {} -> ""
        MString {} -> ""
        _          -> "\t"

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
