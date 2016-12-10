{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Language.Epilog.MIPS.MIPS
  ( MIPS (..)
  , Emips (..)
  , BOp (..)
  , Register (Zero, Scratch, ScratchF, GP, SP, FP, RA)
  , num
  , snum
  , isFloatReg
  , Label
  , emitMIPS
  , nGeneralRegs
  , nFloatRegs
  , nTotalRegs
  , mkRegister
  , mkFloatR
  , mkGeneral
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC (BOp (..), Constant (..), Label (..),
                                         divZeroLabel)
import qualified Language.Epilog.IR.TAC as IR
--------------------------------------------------------------------------------
import           Safe                   (atDef)
import           Control.Monad.RWS      (RWS, evalRWS, modify, gets, tell)
import           Data.Text              (Text, pack)
import qualified Prelude                as Pre (show)
import           Prelude                hiding (show)
--------------------------------------------------------------------------------

emitMIPS :: ([Int32], Seq MIPS) -> Text
emitMIPS (sss, mips) = snd $ evalRWS (emips mips) () sss

class Emips a where
  emips :: a -> RWS () Text [Int32] ()

instance (Emips a, Foldable f) => Emips (f a) where
  emips = mapM_ emips

instance Emips Label where
  emips = tpack . IR.lblstr

show :: Show a => a -> Text
show = pack . Pre.show

chow :: Show a => a -> RWS r Text s ()
chow = tell . show

tpack :: String -> RWS r Text s ()
tpack = tell . pack

--------------------------------------------------------------------------------

data Register -- = Zero | V Int | A Int | T Int | S Int | GP | SP | FP | RA
  = Zero
  | General  {  num :: Word32 }
  | FloatR   {  num :: Word32 } -- Actually holds (n + nGeneralRegs), i.e., FloatR 30 == $f9
  | Scratch  { snum :: Word32 } -- different function since Scratch and ScratchF
  | ScratchF { snum :: Word32 } -- registers don't have descriptors.
  | GP
  | SP
  | FP
  | RA
  deriving (Eq, Show, Read, Ord)


generalRegs, floatRegs, scratchRegs, scratchFloatRegs :: [Text]
generalRegs = ("$" <>) <$>
  [ "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6"
  , "t7", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8"
  , "t9" ]
floatRegs = ("$f" <>) <$>
  [        "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9"
  , "10", "11"      , "13", "14", "15", "16", "17", "18", "19"
  , "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"
  , "30", "31" ] 
scratchRegs = ("$" <>) <$>
  ["v0", "v1", "a0"]
scratchFloatRegs = ("$f" <>) <$>
  [  "0", "12" ]

nGeneralRegs, nFloatRegs, nTotalRegs :: Word32
nGeneralRegs = fromIntegral . length $ generalRegs
nFloatRegs   = fromIntegral . length $ floatRegs
nTotalRegs   = nGeneralRegs + nFloatRegs

mkGeneral, mkFloatR, mkRegister :: Word32 -> Register
mkGeneral i
  | 0 <= i && i < nGeneralRegs         = General i
  | otherwise = error $ "Invalid general register number " <> Pre.show i

mkFloatR i
  | nGeneralRegs <= i && i < nTotalRegs = FloatR  i
  | otherwise = error $ "Invalid float register number " <> Pre.show i

mkRegister i
  | 0 <= i && i < nGeneralRegs         = General i
  | nGeneralRegs <= i && i < nTotalRegs = FloatR  i
  | otherwise = error $ "Invalid register number " <> Pre.show i

isFloatReg :: Register -> Bool
isFloatReg = \case
  FloatR {}   -> True
  ScratchF {} -> True
  _ -> False

instance Emips Register where
  emips Zero = tell "$0"

  emips (General n) = tell $ atDef
    (internal $ "no general use register " <> Pre.show n)
    generalRegs
    (fromIntegral n)

  emips (FloatR n) = tell $ atDef 
    (internal $ "no float use register " <> Pre.show n)
    floatRegs
    (fromIntegral (n - nGeneralRegs))

  emips (Scratch n) = tell $ atDef
    (internal $ "can't scratch" <> Pre.show n)
    scratchRegs
    (fromIntegral n)

  emips (ScratchF n) = tell $ atDef
    (internal $ "can't float scratch" <> Pre.show n)
    scratchFloatRegs
    (fromIntegral n)

  emips GP = tell "$gp"
  emips SP = tell "$sp"
  emips FP = tell "$fp"
  emips RA = tell "$ra"

--------------------------------------------------------------------------------
instance Emips Constant where
  emips = tell . \case
    IC a -> show a
    FC f -> show f
    BC b -> if b then "1" else "0"
    CC c -> show c

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
  | I2F     Register Register
  | I2Fi    Register Constant
  | F2I     Register Register
  | F2Ii    Register Constant
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
  | Clts    Register Register
  | Cles    Register Register
  | Ceqs    Register Register
  -- Terminators
  | Beq  Register Register Label
  | Bne  Register Register Label
  | Bc1t Label
  | Bc1f Label
  | J    Label
  | Jr   Register
  | Jal  String

  | MIPSProlog
  deriving (Eq, Show, Read)

instance Emips MIPS where
  emips l = tell (tab l) >> aux l >> tell "\n"
    where 
      tab = \case
        Comment {} -> ""
        MLabel  {} -> ""
        Data    {} -> ""
        MString {} -> ""
        F2Ii    {} -> ""
        _          -> "\t"

      aux = \case
        Comment str -> tell $ "# " <> pack str
        MLabel lbl  -> emips lbl >> tell ":"
        Data dName dSpace ->
          tpack dName >> tell ": .space " >> chow dSpace
        MString dName dString ->
          tpack dName >> tell ": .asciiz " >> chow dString
        Align       -> tell ".align 2"
        DataSection -> tell ".data"
        TextSection -> tell ".text"
        Global name -> tell $ ".globl " <> pack name
        BinOp  op r1 r2 r3 -> case op of
          DivI -> 
            emips (Beq r3 Zero divZeroLabel) >> 
            tpack "\t" >> emips DivI >> 
            emips r1 >> tpack ", " >>
            emips r2 >> tpack ", " >>
            emips r3 
          RemI ->
            emips (Beq r3 Zero divZeroLabel) >> 
            tpack "\t" >> emips DivI >> 
            emips r2 >> tpack ", " >>
            emips r3 >>
            tpack "\n\tmfhi " >>
            emips r1

          _ -> 
            emips op >> 
            emips r1 >> tpack ", " >>
            emips r2 >> tpack ", " >>
            emips r3 

        BinOpi op r1 r2 c -> case op of
          DivI -> 
            emips DivI >> 
            emips r1 >> tpack ", " >>
            emips r2 >> tpack ", " >>
            emips c 

          RemI -> 
            emips DivI >> 
            emips (Scratch 0) >> tpack ", " >>
            emips r2 >> tpack ", " >>
            emips c >>
            tpack "\n\tmfhi " >>
            emips r1

          _ -> 
            emips op >> 
            emips r1 >> tpack ", " >>
            emips r2 >> tpack ", " >>
            emips c 

        I2F  fr gr ->
          tpack "mtc1 " >> 
          emips gr >> tpack ", " >> emips fr >>
          tpack "\n\tcvt.s.w " >>
          emips fr >> tpack ", " >> emips fr

        I2Fi fr c ->
          emips (LoadI (Scratch 0) c) >>
          tpack "\tmtc1 " >> 
          emips (Scratch 0) >> tpack ", " >> emips fr >>
          tpack "\n\tcvt.s.w " >>
          emips fr >> tpack ", " >> emips fr

        F2I  gr fr ->
          tpack "cvt.w.s " >>
          emips (ScratchF 0) >> tpack ", " >> emips fr >>
          tpack "\n\tmfc1 " >> 
          emips gr >> tpack ", " >> emips (ScratchF 0)

        F2Ii gr c ->
          emips (LoadFI (ScratchF 0) c) >>
          tpack "\tcvt.w.s " >>
          emips (ScratchF 0) >> tpack ", " >> emips (ScratchF 0) >>
          tpack "\n\tmfc1 " >> 
          emips gr >> tpack ", " >> emips (ScratchF 0)



        LoadA r1 s -> tpack "la " >> emips r1 >> tpack ", " >> tpack s

        LoadI r1 i -> tpack "li " >> emips r1 >> tpack ", " >> emips i

        LoadW r1 (c,r2) -> 
          tpack "lw " >> emips r1 >> tpack ", " >> chow c >> 
          tpack "(" >> emips r2 >> tpack ")"

        LoadWG r name ->
          tpack "lw " >> emips r >> tpack ", " >> tpack name

        LoadFI r1 i -> tpack "li.s " >> emips r1 >> tpack ", " >> emips i

        LoadF r1 (c,r2) ->
          tpack "l.s " >> emips r1 >> tpack ", " >> chow c >> 
          tpack "(" >> emips r2 >> tpack ")"

        LoadFG r name ->
          tpack "l.s " >> emips r >> tpack ", " >> tpack name

        Move r1 r2 ->
          tpack "move " >> 
          emips r1 >> tpack ", " >>
          emips r2

        StoreW r1 (c,r2) ->
          tpack "sw " >> emips r1 >> tpack ", " >> chow c >> 
          tpack "(" >> emips r2 >> tpack ")"
        
        StoreWG r name ->
          tpack "sw " >> emips r >> tpack ", " >> tpack name

        StoreF r1 (c,r2) ->
          tpack "s.s " >> emips r1 >> tpack ", " >> chow c >> 
          tpack "(" >> emips r2 >> tpack ")"
        
        StoreFG r name ->
          tpack "s.s " >> emips r >> tpack ", " >> tpack name

        Syscall -> tpack "syscall"

        Slt r1 r2 r3 ->
          tpack "slt " >> 
          emips r1 >> tpack ", " >>
          emips r2 >> tpack ", " >>
          emips r3 

        Clts r1 r2 ->
          tpack "c.lt.s " >>
          emips r1 >> tpack ", " >>
          emips r2

        Cles r1 r2 ->
          tpack "c.le.s " >>
          emips r1 >> tpack ", " >>
          emips r2

        Ceqs r1 r2 ->
          tpack "c.eq.s " >>
          emips r1 >> tpack ", " >>
          emips r2

        Beq  r1 r2 label ->
          tpack "beq " >> 
          emips r1 >> tpack ", " >>
          emips r2 >> tpack ", " >>
          emips label

        Bne  r1 r2 label ->
          tpack "bne " >> 
          emips r1 >> tpack ", " >>
          emips r2 >> tpack ", " >>
          emips label

        Bc1t r  -> tpack "bc1t " >> emips r
        Bc1f r  -> tpack "bc1f " >> emips r
        J label -> tpack "j "    >> emips label
        Jr r    -> tpack "jr "   >> emips r
        Jal str -> tell $ "jal " <> pack str

        MIPSProlog -> do
          n <- gets head
          emips . Comment $ "Prolog " <> Pre.show n
          emips $ Move FP SP
          emips $ StoreW RA (4, FP)
          emips $ BinOpi SubI SP SP (IC . fromIntegral $ n)
          modify tail

instance Emips BOp where
  emips op = aux >> tell " "
    where 
      aux = tell $ case op of
        AddI -> "add"
        SubI -> "sub"
        MulI -> "mul"
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
