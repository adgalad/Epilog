{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase     #-}

module Language.Epilog.MIPS.Program
  ( mipsProgram
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import qualified Language.Epilog.IR.TAC        as TAC
import           Language.Epilog.MIPS.Monad
import           Language.Epilog.MIPS.MIPS     
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type          (sizeT)
--------------------------------------------------------------------------------
import           Control.Lens                  (use, (.=),(%=))
import qualified Data.Sequence                 as Seq (empty)
import qualified Data.Map                      as Map (toList, empty, insert)
--------------------------------------------------------------------------------

mipsProgram :: TAC.Program -> MIPSMonad Program
mipsProgram TAC.Program { TAC.datas, TAC.modules } = do
  forM_ modules mipsModule
  Program datas <$> use blocks

mipsModule :: TAC.Module -> MIPSMonad ()
mipsModule TAC.Module {TAC.mBlocks} = forM_ mBlocks $ \m -> do
  mipsBlock m
  resetRegDescrips
  variables .= Map.empty

mipsBlock :: TAC.Block -> MIPSMonad ()
mipsBlock TAC.Block {TAC.lbl, TAC.tacs, TAC.term} = do
  mapM_ mipsTAC tacs
  mipsTerminator term
  code <- use instructions
  instructions .= Seq.empty
  let b = Block 
          { bLabel = lbl
          , bCode  = code }

  void $ blocks |>= b


mipsTerminator :: TAC.Terminator -> MIPSMonad ()
mipsTerminator = \case 
  TAC.Br dest -> addMIPS $ J dest
  
  TAC.IfBr cond trueDest falseDest -> do 
    addMIPS $ Bne (V 0) Zero trueDest -- TODO: (V 0) <- cond
    addMIPS $ J falseDest 

  TAC.CondBr rel op0 op1 trueDest falseDest -> do 
    case rel of

       -- LTF ->  c.lt.s $f1, $f2 ; bc1t l`

       -- LEF ->  c.le.s $f1, $f2 ; bc1t l`

       -- GTF ->  c.lt.s $f2, $f1 ; bc1t l`

       -- GEF ->  c.le.s $f2, $f1 ; bc1t l`

       -- EQF ->  c.eq.s $f1, $f2 ; bc1t l`

       -- NEF ->  c.eq.s $f1, $f2 ; bc1f l`


      TAC.LTI -> do
        addMIPS $ Slt (V 0) (V 1) (V 2)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Bne (V 0) Zero trueDest

      TAC.LEI -> do
        addMIPS $ Slt (V 0) (V 1) (V 2)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Beq (V 0) Zero trueDest

      TAC.GTI -> do
        addMIPS $ Slt (V 0) (V 2) (V 1)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Bne (V 0) Zero trueDest

      TAC.GEI -> do
        addMIPS $ Slt (V 0) (V 1) (V 2)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Beq (V 0) Zero trueDest

      TAC.EQI -> addMIPS $ Beq (V 1) (V 2) trueDest-- TODO: V1 <- op0, V2 <- op1

      TAC.NEI -> addMIPS $ Bne (V 1) (V 2) trueDest-- TODO: V1 <- op0, V2 <- op1

      TAC.FAI -> do
        addMIPS $ BinOp DivI (V 0) (V 2) (V 1)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Bne (V 1) (V 2) trueDest        -- TODO: V1 <- op0, V2 <- op1

      TAC.NFI -> do
        addMIPS $ BinOp DivI (V 0) (V 2) (V 1)-- TODO: V1 <- op0, V2 <- op1
        addMIPS $ Beq (V 1) (V 2) trueDest -- TODO: V1 <- op0, V2 <- op1

    addMIPS $ J falseDest


  TAC.Return -> do
    addMIPS $ Jr RA 
  TAC.Exit -> do
    addMIPS $ LoadI (V 0) 10
    addMIPS Syscall

  _ -> addMIPS $ Jal "Nada"

mipsTAC :: TAC.TAC -> MIPSMonad ()
mipsTAC = \case 
  TAC.Comment str -> addMIPS $ Comment str

  TAC.Var _ name offs size -> do
    let 
      var = (VarDescrip Nothing False (Local offs))
    variables %= Map.insert (TAC.R name) var


  -- op := operation -> undefined

  -- op1 :=# (op2, op3) ->

  -- (Operand, Operand) :#= Operand ->

  -- Operand :=* Operand ->

  -- Operand :*= Operand ->


  -- Operand :=& Operand ->


  -- Param Operand ->

  -- RefParam Operand ->

  TAC.Call proc -> do 
    addMIPS $ BinOpi SubI SP SP (IC $ 12)
    addMIPS $ StoreW FP (0, SP)
    addMIPS . Jal $ "_proc_" <> proc

  
  -- Operand :<- Function ->


  TAC.Cleanup n -> do 
    addMIPS $ LoadW FP (0, FP)
    addMIPS $ BinOpi AddI SP SP (IC $ n+12)


  TAC.Prolog n -> do
    addMIPS $ Move FP SP
    addMIPS $ BinOpi SubI SP SP (IC $ n)


  TAC.Epilog _ -> do 
    addMIPS $ StoreW RA (4, FP)
    addMIPS $ Move SP FP

  -- Answer op ->

  c -> addMIPS $ Comment (TAC.emit c)