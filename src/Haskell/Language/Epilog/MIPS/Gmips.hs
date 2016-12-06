{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase     #-}

module Language.Epilog.MIPS.Gmips
  ( Gmips (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC      hiding (Comment, Constant (..))
import qualified Language.Epilog.IR.TAC      as TAC (TAC (Comment), Constant (..))
import           Language.Epilog.MIPS.Monad
import           Language.Epilog.MIPS.MIPS     
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type          (sizeT)
--------------------------------------------------------------------------------
import           Control.Lens                  (use, (.=),(%=))
import qualified Data.Sequence                 as Seq (empty)
import qualified Data.Map                      as Map (toList, empty, insert)
--------------------------------------------------------------------------------

spill :: Register
spill = undefined

getReg3 :: TAC -> (Register, Register, Register)
getReg3 t = undefined

getReg2 :: TAC -> (Register, Register)
getReg2 t = undefined

getReg1 :: TAC -> Register
getReg1 = \case
  _ :=& (R name) -> do
  Param _ -> do
  _ :<- proc -> do
  Answer op -> do
  tac -> internal $ "Get reg of " <> emit tac


-- copy :: Operand -> Operand -> ()
-- copy dest orig = undefined

class Gmips a where
  gmips :: a -> MIPSMonad ()

instance Gmips Program where
  gmips Program { datas, modules } = do
    unless (null datas) $ do
      tell1 DataSection
      mapM_ gmips datas

    unless (null modules) $ do
      tell1 TextSection
      mapM_ gmips modules

instance Gmips Data where
  gmips VarData { dName, dSpace } = do
    tell1 $ Data dName dSpace
  gmips StringData { dName, dString } = do
    tell1 $ MString dName dString

instance Gmips Module where
  gmips Module { mName, mBlocks } = do
    tell1 $ Comment mName
    mapM_ gmips mBlocks
    resetRegDescs        -- TODO
    variables .= Map.empty  -- TODO

instance Gmips Block where
  gmips Block { lbl, tacs, term } = do
    tell1 $ MLabel lbl
    mapM_ gmips tacs
    gmips term

instance Gmips Terminator where
  gmips = \case
    Br dest -> tell1 $ J dest
    
    IfBr cond trueDest falseDest -> do 
      tell1 $ Bne (Scratch 0) Zero trueDest -- TODO: (Scratch 0) <- cond !!!
      tell1 $ J falseDest 

    CondBr rel op0 op1 trueDest falseDest -> do 
      case rel of

         -- LTF ->  c.lt.s $f1, $f2 ; bc1t l`

         -- LEF ->  c.le.s $f1, $f2 ; bc1t l`

         -- GTF ->  c.lt.s $f2, $f1 ; bc1t l`

         -- GEF ->  c.le.s $f2, $f1 ; bc1t l`

         -- EQF ->  c.eq.s $f1, $f2 ; bc1t l`

         -- NEF ->  c.eq.s $f1, $f2 ; bc1f l`


        LTI -> do
          tell1 $ Slt (Scratch 0) (Scratch 1) (Scratch 2)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Bne (Scratch 0) Zero trueDest

        LEI -> do
          tell1 $ Slt (Scratch 0) (Scratch 1) (Scratch 2)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Beq (Scratch 0) Zero trueDest

        GTI -> do
          tell1 $ Slt (Scratch 0) (Scratch 2) (Scratch 1)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Bne (Scratch 0) Zero trueDest

        GEI -> do
          tell1 $ Slt (Scratch 0) (Scratch 1) (Scratch 2)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Beq (Scratch 0) Zero trueDest

        EQI -> 
          tell1 $ Beq (Scratch 1) (Scratch 2) trueDest-- TODO: V1 <- op0, V2 <- op1

        NEI -> 
          tell1 $ Bne (Scratch 1) (Scratch 2) trueDest-- TODO: V1 <- op0, V2 <- op1

        FAI -> do
          tell1 $ BinOp RemI (Scratch 0) (Scratch 2) (Scratch 1)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Bne (Scratch 1) (Scratch 2) trueDest        -- TODO: V1 <- op0, V2 <- op1

        NFI -> do
          tell1 $ BinOp RemI (Scratch 0) (Scratch 2) (Scratch 1)-- TODO: V1 <- op0, V2 <- op1
          tell1 $ Beq (Scratch 1) (Scratch 2) trueDest -- TODO: V1 <- op0, V2 <- op1

      tell1 $ J falseDest

    Return -> do
      tell1 $ Jr RA

    Exit -> do
      tell1 $ LoadI (Scratch 0) 10
      tell1 Syscall

    _ -> internal "wut"

instance Gmips TAC where
  gmips tac = case tac of
    TAC.Comment str -> tell1 $ Comment str

    Var _ name offs _ -> 

      home %= Map.insert name offs


    op := operation -> case operation of
      B o l r -> do
        (x, y, z) <- getReg3 tac
        tell $ BinOp o x y z
        -- TODO: conmutatividad con constantes

      U o u -> do
        (x, y) <- getReg2 tac
        case o of
          NegF -> undefined -- jeje que es flout?
          NegI -> tell $ BinOp SubI x Zero y
          BNot -> tell $ BinOp Xor  x Zero y
          Id   -> pure ()

    _ :=# (_, C (IC n)) -> do
      (x, y) <- getReg2 tac
      tell1 $ LoadW x (n, y)

    _ :=# (_, _) -> do
      (x, y, z) <- getReg3
      tell
        [ BinOp AddI x y z
        , LoadW x (0, x) ]

    (_, C (IC n)) :#= _ -> do
      (x, y) <- getReg2 tac
      tell1 $ StoreW y (n, x)

    (_, _) :#= _ -> do
      (x, y, z) <- getReg3 tac
      tell
        [ BinOp AddI (Scratch 0) x y
        , StoreW z (0, (Scratch 0)) ]

    _ :=* _ -> do
      (x, y) <- getReg2 tac
      tell1 $ LoadW x (0, y) 

    _ :*= _ -> do
      (x, y) <- getReg2 tac
      tell1 $ StoreW y (0, x) 

    _ :=& (R name) -> do
      x <- getReg1 tac
      use home
      tell1 $ case name `Map.lookup` home of
        Nothing     -> LoadA x name
        Just offset -> BinOpi AddI x FP offset

    _ :=& _ -> internal $ "Invalid address-of"

    Param _ -> do
      x <- getReg1 tac
      tell 
        [ BinOpi AddI SP SP (-4)
        , StoreW x (0, SP) ]

    RefParam _ -> do
      use home
      tell
        [ BinOpi AddI SP SP (-4)
        , case name `Map.lookup` home of
            Nothing     -> LoadA (Scratch 0) name
            Just offset -> BinOpi AddI (Scratch 0) FP offset
        , StoreW (Scratch 0) (0, SP) ]

    Call proc -> tell 
      [ BinOpi SubI SP SP (IC $ 12)
      , StoreW FP (0, SP)
      , Jal $ "proc_" <> proc 
      -- prolog
      -- ???
      -- profit
      -- epilog
      -- Cleanup
      ]

    _ :<- proc -> do
      x <- getReg1 tac
      tell
        [ BinOpi SubI SP SP (IC $ 12)
        , StoreW FP (0, SP)
        , Jal $ "proc_" <> proc
        -- prolog
        -- ???
        -- profit
        -- epilog
        , LoadW x (8, FP) 
        -- Cleanup
        ]

    Cleanup n -> tell 
      [ LoadW FP (0, FP)
      , BinOpi AddI SP SP (IC $ n+12) ]


    Prolog n -> tell
      [ Move FP SP
      , StoreW RA (4, FP)
      , BinOpi SubI SP SP (IC $ n) ]


    Epilog _ -> tell 
      [ LoadW RA (4, FP)
      , Move SP FP ]

    Answer op -> do
      x <- getReg1 tac
      tell1 $ StoreW x (8, FP)

    c -> tell1 $ Comment (emit c)
