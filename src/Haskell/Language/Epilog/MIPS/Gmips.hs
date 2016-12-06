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
import           Control.Lens                  (use, (.=),(%=), (+~), (+=), 
                                                (<-=), ix, (&))
import           Data.Array.IO                 (IOArray)
import           Data.Array.MArray             (readArray, writeArray)
import qualified Data.Sequence                 as Seq (empty)
import qualified Data.Map                      as Map (toList, empty, insert, lookup)
--------------------------------------------------------------------------------

spill :: Register
spill = undefined

getReg3 :: TAC -> MIPSMonad (Register, Register, Register)
getReg3 t = undefined

getReg2 :: TAC -> MIPSMonad (Register, Register)
getReg2 t = do 
  vd <- use variables
  h  <- use home
  rd <- use registers 
  let 
    (op1, op2) = case t of 
      (op1 := U o op2)   -> (op1,op2)
      op1 :=# (op2, C _) -> (op1,op2)
      (op1, C _) :#= op2 -> (op1,op2)
      op1 :=* op2        -> (op1,op2)
      op1 :*= op2        -> (op1,op2)
      _ -> internal $ "wut? " <> emit t 

  if op1 == op2
    then getReg1 t >>= \r -> pure (r,r)
    else undefined


  


getReg1 :: TAC -> MIPSMonad Register
getReg1 t = do 
  vd <- use variables
  h  <- use home
  rd <- use registers
  let 
    op = case t of
      op :=& (R name) -> op
      Param op        -> op
      op :<- _        -> op
      Answer op       -> op
      -- getreg2 using getreg1
      op := U o _     -> op
      op :=# (_, C _) -> op
      (op, C _) :#= _ -> op
      op :=* _        -> op
      op :*= _        -> op
      _ -> internal $ "wut? " <> emit t 

  case op `Map.lookup` vd of
    Nothing -> do 
      
      case op `Map.lookup` h of
        Nothing -> do 
          n <- bestReg False
          case op of 
            T _ -> do
              variables %= Map.insert op n
              liftIO $ writeArray rd n (RegDesc [op] 10 False)
            _ -> internal $ "Operand not found: " <> emit op
          pure $ General n

        Just offset -> do
          General <$> case t of
            op :=& (R name) -> bestReg False
            op :<- _        -> bestReg False
            _ -> do
              n <- bestReg True
              tell1 $ LoadW (General n) (offset, FP)
              liftIO $ writeArray rd n (RegDesc [op] 1 False)
              pure n

    Just regNum -> do
      pure $ General regNum
    where 
      bestReg :: Bool -> MIPSMonad Word32
      bestReg spillear = do
        rd <- use registers
        h  <- use home
        n  <- min' rd
        d@RegDesc {values, ss, dirty} <- liftIO $ readArray rd (fromIntegral n)

        when (dirty && spillear) . forM_ values $ \v -> do
          case v `Map.lookup` h of
            Nothing     -> case v of 
              (R name) -> tell1 $ StoreWG (General n) name
              
              t@(T _)  -> do 
                tell [ BinOpi AddI SP SP (IC $ -4)
                     , StoreW (General n) (0, SP) ]
                toffs <- vsp <-= 4
                home %= Map.insert t toffs
              _        -> internal "trying to save a constant operand as global"
            Just offset ->
              tell1 $ StoreW (General n) (offset, FP)
        pure n

      min' :: IOArray Word32 RegDesc -> MIPSMonad Word32
      min' regs = do
        r0 <- liftIO $ readArray regs 0
        if not (dirty r0) || ss r0 == 0
          then pure 0
          else min'' 1 (0, ss r0)

        where
          min'' :: Word32 -> (Word32, Word32) -> MIPSMonad Word32
          min'' 21 (b, bc) = pure b
          min'' i  (b, bc) = do
            ri <- liftIO $ readArray regs i
            if not (dirty ri) || ss ri == 0
              then pure i
              else min'' (i+1) $ if ss ri < bc
                then (i, ss ri)
                else (b, bc)

contantToMips :: TAC.Constant -> Constant 
contantToMips = \case
  TAC.IC a -> IC a
  TAC.FC f -> FC f
  TAC.BC b -> IC $ if b then 1 else 0
  TAC.CC c -> CC c


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
      tell1 $ LoadI (Scratch 0) (IC 10)
      tell1 Syscall

    _ -> internal "wut"

instance Gmips TAC where
  gmips tac = case tac of
    TAC.Comment str -> tell1 $ Comment str

    Var _ name offs _ -> 

      home %= Map.insert (R name) offs


    op := operation -> case operation of
      B o l r -> do
        (x, y, z) <- getReg3 tac
        tell1 $ BinOp o x y z
        -- TODO: conmutatividad con constantes

      U o u -> do
        (x, y) <- getReg2 tac
        case o of
          NegF -> undefined -- jeje que es flout?
          NegI -> tell1 $ BinOp SubI x Zero y
          BNot -> tell1 $ BinOp BXor x Zero y
          Id   -> pure ()

    -- _ :=# (_, C (TAC.IC n)) -> do
    --   (x, y) <- getReg2 tac
    --   tell1 $ LoadW x (n, y)

    -- _ :=# (_, _) -> do
    --   (x, y, z) <- getReg3 tac
    --   tell
    --     [ BinOp AddI x y z
    --     , LoadW x (0, x) ]

    -- (_, C (TAC.IC n)) :#= _ -> do
    --   (x, y) <- getReg2 tac
    --   tell1 $ StoreW y (n, x)

    -- (_, _) :#= _ -> do
    --   (x, y, z) <- getReg3 tac
    --   tell
    --     [ BinOp AddI (Scratch 0) x y
    --     , StoreW z (0, (Scratch 0)) ]

    _ :=* _ -> do
      (x, y) <- getReg2 tac
      tell1 $ LoadW x (0, y) 

    _ :*= _ -> do
      (x, y) <- getReg2 tac
      tell1 $ StoreW y (0, x) 

    _ :=& o@(R name) -> do
      x <- getReg1 tac
      h <- use home
      tell1 $ case o `Map.lookup` h of
        Nothing     -> LoadA x name
        Just offset -> BinOpi AddI x FP (IC offset)

    Param (C c) -> case c of
      TAC.FC _ -> undefined
      _    -> tell 
        [ LoadI (Scratch 0) $ contantToMips c 
        , BinOpi AddI SP SP (IC $ -4)
        , StoreW (Scratch 0) (0, SP) ]

    _ :=& _ -> internal $ "Invalid address-of"

    _ :=@ (r@(R name), C (TAC.IC n)) -> do
      h <- use home
      x <- getReg1 tac
      case r `Map.lookup` h of
        -- Global
        Nothing -> if n == 0 
          then tell1 $ LoadA x name
          else tell 
            [ LoadA (Scratch 0) name 
            , BinOpi AddI x (Scratch 0) (IC n) ]

        -- Local
        Just offset -> tell1 $ BinOpi AddI x FP (IC $ n + offset)

    _ :=@ (r@(R name), _) -> do
      h <- use home
      (x, y) <- getReg2 tac
      case r `Map.lookup` h of
        -- Global
        Nothing -> tell 
          [ LoadA (Scratch 0) name 
          , BinOp AddI x (Scratch 0) y ]

        -- Local
        Just offset -> tell
          [ BinOpi AddI (Scratch 0) FP (IC offset)
          , BinOp  AddI x (Scratch 0) y ]

    a :=@ (b@(T num), c) -> gmips (a := B AddI b c)

    Param _ -> do
      x <- getReg1 tac
      tell 
        [ BinOpi AddI SP SP (IC $ -4)
        , StoreW x (0, SP) ]

    RefParam o@(R name) -> do
      h <- use home
      tell
        [ BinOpi AddI SP SP (IC $ -4)
        , case o `Map.lookup` h of
            Nothing     -> LoadA (Scratch 0) name
            Just offset -> BinOpi AddI (Scratch 0) FP (IC offset)
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

    op :<- proc -> do
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
      , BinOpi AddI SP SP (IC . fromIntegral $ n+12) ]


    Prolog n -> do
      tell [ Move FP SP
           , StoreW RA (4, FP)
           , BinOpi SubI SP SP (IC . fromIntegral $ n) ]
      vsp .= fromIntegral (-n)


    Epilog _ -> do 
      tell [ LoadW RA (4, FP)
           , Move SP FP ]


    Answer (C c) -> case c of
      TAC.FC _ -> undefined
      _    -> tell 
        [ LoadI (Scratch 0) $ contantToMips c 
        , StoreW (Scratch 0) (8, FP) ]

    Answer _ -> do
      x <- getReg1 tac
      tell1 $ StoreW x (8, FP)

    c -> tell1 $ Comment (emit c)
