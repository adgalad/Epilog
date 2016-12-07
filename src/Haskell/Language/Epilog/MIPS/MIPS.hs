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
  , Label
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC (BOp (..), Constant (..), Label (..),
                                         divZeroLabel, emit)
import qualified Language.Epilog.IR.TAC as IR
--------------------------------------------------------------------------------
import           Data.List              (intercalate)
import           Safe                   (atDef)
--------------------------------------------------------------------------------

class Emips a where
  emips :: a -> String

instance Emips Label where
  emips = IR.lblstr

--------------------------------------------------------------------------------

data Register -- = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  = Zero
  | Scratch { num :: Word32 }
  | General { num :: Word32 }
  | ScratchF{ num :: Word32 }
  | FloatP  { num :: Word32 }
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

  emips (FloatP n) = "$f" <> atDef 
    (internal $ "no float use register " <> show n)
    [  "1", "2", "3", "4", "5", "6", "7", "8", "9"
    , "10", "11", "13", "14", "15", "16", "17", "18" 
    , "19", "20", "21", "22", "23", "24", "25", "26" 
    , "27", "28", "29", "30", "31" ] 
    (fromIntegral n)

  emips (ScratchF n) = atDef
    (internal $ "can't float scratch" <> show n)
    ["$f0", "$f12" ]
    (fromIntegral n)

  emips GP = "$gp"
  emips SP = "$sp"
  emips FP = "$fp"
  emips RA = "$ra"

--------------------------------------------------------------------------------
instance Emips Constant where
  emips = \case
    IC a -> show a
    FC f -> show f
    BC b -> if b then "1" else "0"
    CC c -> show c

--------------------------------------------------------------------------------
instance (Emips a, Foldable f) => Emips (f a) where
  emips = concat . fmap emips . toList

--------------------------------------------------------------------------------
data MIPS
  = Comment String
  | MLabel   Label
  -- Sections
  | Align
  | DataSection
  | TextSection
  | Global String
  -- Declarations
  | Data    Name Int32
  | MString Name String
  -- Instructions
  | BinOp   BOp Register Register Register
  | BinOpi  BOp Register Register Constant
  | LoadA   Register String
  | LoadI   Register Constant
  | LoadW   Register (Int32, Register)
  | LoadWG  Register String
  | LoadFI  Register Constant
  | LoadF   Register (Int32, Register)
  | LoadFG  Register String
  | Move    Register Register
  | StoreW  Register (Int32, Register)
  | StoreWG Register String
  | StoreF  Register (Int32, Register)
  | StoreFG Register String
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
    MLabel lbl  -> emit lbl <> ":"
    Data dName dSpace ->
      dName <> ": .space " <> show dSpace
    MString dName dString ->
      dName <> ": .asciiz " <> show dString
    Align       -> ".align 2"
    DataSection -> ".data"
    TextSection -> ".text"
    Global name -> ".globl " <> name
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
        [         emips DivI <> intercalate ", " [emips (Scratch 0), emips r2, emips c]
        , "\t" <> "mfhi " <> emips r1 ]

      _ -> emips op <> intercalate ", " (fmap emips [r1, r2] <> [emips c])

    LoadA r1 s -> "la " <> emips r1 <> ", " <> s

    LoadI r1 i -> "li " <> emips r1 <> ", " <> emips i

    LoadW r1 (c,r2) ->
      "lw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

    LoadWG r name ->
      "lw " <> emips r <> ", " <> name

    LoadFI r1 i -> "li.s " <> emips r1 <> ", " <> emips i

    LoadF r1 (c,r2) ->
      "l.s " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"

    LoadFG r name ->
      "l.s " <> emips r <> ", " <> name

    Move r1 r2 ->
      "move " <> intercalate ", " (emips <$> [r1,r2])

    StoreW r1 (c,r2) ->
      "sw " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"
    
    StoreWG r name ->
      "sw " <> emips r <> ", " <> name

    StoreF r1 (c,r2) ->
      "s.s " <> emips r1 <> ", " <> show c <> "(" <> emips r2 <> ")"
    
    StoreFG r name ->
      "s.s " <> emips r <> ", " <> name

    Syscall ->
      "syscall"



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
    t -> internal $ show t

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
    BSR  -> "srl"
    BAnd -> "and"
    BOr  -> "or"
    BXor -> "xor"
