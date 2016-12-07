{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.MIPS.Gmips
  ( Gmips (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC     hiding (Comment)
import qualified Language.Epilog.IR.TAC     as TAC (TAC (Comment))
import           Language.Epilog.MIPS.MIPS
import           Language.Epilog.MIPS.Monad
--------------------------------------------------------------------------------
import           Control.Lens               (use, (%=), (.=), (<-=))
import           Data.Array.IO              (IOArray)
import           Data.Array.MArray          (readArray, writeArray)
import qualified Data.Map                   as Map (empty, insert, lookup)
import           Data.Maybe                 (fromMaybe)
--------------------------------------------------------------------------------
import           Debug.Trace                (traceM)
--------------------------------------------------------------------------------

-- spill :: Register
-- spill = undefined

data TAC'
  = IT TAC
  | TT Terminator

instance Emit TAC' where
  emit = \case
    IT i -> emit i
    TT t -> emit t

class GetReg i where
  getReg1 :: i -> MIPSMonad (Register)
  getReg2 :: i -> MIPSMonad (Register, Register)
  getReg3 :: i -> MIPSMonad (Register, Register, Register)

instance GetReg TAC where
  getReg1 = getReg1' . IT
  getReg2 = getReg2' . IT
  getReg3 = getReg3' . IT

instance GetReg Terminator where
  getReg1 = getReg1' . TT
  getReg2 = getReg2' . TT
  getReg3 = getReg3' . TT

instance GetReg TAC' where
  getReg1 = getReg1'
  getReg2 = getReg2'
  getReg3 = getReg3'

getReg3' :: TAC' -> MIPSMonad (Register, Register, Register)
getReg3' t = do
  vd <- use variables
  h  <- use home
  rd <- use registers
  let
    (op1, op2, op3) = case t of
      IT (a := B o b c) -> (a, b, c)
      IT (a :=# (b, c)) -> (a, b, c)
      -- (_, _) :#= _ -> (a, b, c) -- FIXME!!!

  if
    | op1 == op2 && op1 == op3 -> do
        x <- case op1 `Map.lookup` vd of
          Nothing -> do
            x <- freshReg
            load x op1
            pure x
          Just x -> pure x
        pure (General x,General x,General x)

    | op1 == op2 -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> freshReg
        Just x  -> pure x

      y <- case op3 `Map.lookup` vd of
        Nothing -> do
          y <- freshReg' [x]
          load y op3
          pure y
        Just y -> pure y

      pure (General x, General x, General y)

    | op1 == op3 -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> freshReg
        Just x  -> pure x

      y <- case op2 `Map.lookup` vd of
        Nothing -> do
          y <- freshReg' [x]
          load y op2
          pure y
        Just y -> pure y

      pure (General x, General y, General x)

    | otherwise -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> freshReg
        Just x  -> pure x

      y <- case op2 `Map.lookup` vd of
        Nothing -> do
          y <- freshReg' [x]
          load y op2
          pure y
        Just y -> pure y

      z <- case op3 `Map.lookup` vd of
        Nothing -> do
          z <- freshReg' [x, y]
          load z op3
          pure z
        Just z -> pure z

      pure (General x, General y, General z)

getReg2' :: TAC' -> MIPSMonad (Register, Register)
getReg2' t = do
  vd <- use variables
  h  <- use home
  rd <- use registers
  let
    (op1, op2) = case t of
      IT ((a := U o b)  ) -> (a, b)
      IT (a :=# (b, C _)) -> (a, b)
      IT (a :=* b       ) -> (a, b)
      -- IT ((a, C _) :#= b) -> (a, b) -- FIXME!!!
      -- IT (a :*= b       ) -> (a, b) -- FIXME!!!

  if op1 == op2
    then do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg
          load x op1
          pure x
        Just x -> pure x
      pure (General x, General x)

    else do
      case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg
          case op2 `Map.lookup` vd of
            Just y -> do
              pure (General x, General y)
            Nothing -> do
              
              -- variables %= Map.insert op n
              -- liftIO $ writeArray rd n (RegDesc [op] ss False)

              load x op2
              pure (General x, General x)

        Just x -> do
          case op2 `Map.lookup` vd of
            Just y -> pure (General x, General y)
            Nothing -> do
              y <- freshReg' [x] -- spill x op1
              load y op2
              pure (General x, General y)

load :: Word32 -> Operand -> MIPSMonad ()
load r op = do
  h <- use home
  case op `Map.lookup` h of
    Nothing -> case op of
      R name -> tell1 $ LoadWG (General r) name
      _ -> internal $
        "Reading from Constant or unavailable Temp " <> emit op
    Just offset -> tell1 $ LoadW (General r) (offset, FP)


getReg1' :: TAC' -> MIPSMonad Register
getReg1' t = do
  vd <- use variables
  h  <- use home
  rd <- use registers
  let
    op = case t of
      -- Special case (value)
      IT (Param (C _)   ) -> internal "Constant Param"
      IT (Param x       ) -> x
      IT (Answer (C _)  ) -> internal "Constant Answer"
      IT (Answer x      ) -> x

      IT (x :=& R name  ) -> x
      IT (x :<- _       ) -> x

  case op `Map.lookup` vd of
    Nothing -> do
      n <- freshReg
      ss <- cost op

      variables %= Map.insert op n
      liftIO $ writeArray rd n (RegDesc [op] ss False)

      when (t `reading` op) $ load n op

      pure $ General n

    Just regNum -> do
      pure $ General regNum

cost :: Operand -> MIPSMonad Word32
cost op = do
  h <- use home  
  pure $ if temporary op && isNothing (op `Map.lookup` h) 
    then 10 
    else 1

  where
    temporary :: Operand -> Bool
    temporary = \case
      T _ -> True
      _ -> False

reading :: TAC' -> Operand -> Bool
reading t op = case t of
  IT (Param  x    ) -> x == op
  IT (Answer x    ) -> x == op

  IT (_ :=& _     ) -> False
  IT (_ :<- _     ) -> False
  IT (Call _      ) -> False

  IT (_ := U o x  ) -> x == op
  IT (_ := B o x y) -> x == op || y == op

  IT (_ :=# (x, y)) -> x == op || y == op
  IT ((x, y) :#= z) -> x == op || y == op || z == op
  IT (_ :=* x     ) -> x == op
  IT (x :*= y     ) -> x == op || y == op

-- writing :: TAC' -> Operand -> Bool
-- writing t op = case t of
--   IT (Param  x    ) -> False
--   IT (Answer x    ) -> False

--   IT (x :=& _     ) -> x == op
--   IT (x :<- _     ) -> x == op
--   IT (Call _      ) -> False

--   IT (x := U o _  ) -> x == op
--   IT (x := B o _ _) -> x == op

--   IT (x :=# (_, _)) -> x == op
--   IT ((x, y) :#= z) -> False
--   IT (x :=* _     ) -> x == op
--   IT (_ :*= _     ) -> False



freshRegs :: Int -> MIPSMonad [ Word32 ]
freshRegs 1 = (:[]) <$> freshReg
freshRegs n = do
  rs <- freshRegs (n-1)
  r  <- freshReg' rs
  pure $ r : rs

freshReg :: MIPSMonad Word32
freshReg = freshReg' []

freshReg' :: [ Word32 ] -> MIPSMonad Word32
freshReg' exceptions = do
  rd <- use registers
  h  <- use home
  n  <- fromMaybe
    (internal $ "couldn't find a fresh reg meeting exceptions " <> show exceptions) <$>
    (min' rd)
  d@RegDesc {values, dirty} <- liftIO $ readArray rd n

  when dirty $ do
    liftIO $ writeArray rd n d { dirty = False }
    forM_ values $ \v -> do
      case v `Map.lookup` h of
        Nothing     -> case v of
          (R name) -> tell1 $ StoreWG (General n) name

          temp@(T _)  -> do
            tell [ BinOpi AddI SP SP (IC $ -4)
                 , StoreW (General n) (0, SP) ]
            toffs <- vsp <-= 4
            home %= Map.insert temp toffs

          _        -> internal "trying to save a constant operand as global"
        Just offset ->
          tell1 $ StoreW (General n) (offset, FP)
  pure n

  where
    min' :: IOArray Word32 RegDesc -> MIPSMonad (Maybe Word32)
    min' regs = min'' 0 Nothing


      where
        min'' :: Word32 -> (Maybe (Word32, Word32)) -> MIPSMonad (Maybe Word32)
        min'' 21 (Just (b, bc)) = pure . Just $ b
        min'' 21 Nothing = pure Nothing
        min'' i  mb = if i `elem` exceptions
          then min'' (succ i) mb
          else do
            ri <- liftIO $ readArray regs i
            if (not (dirty ri) || ss ri == 0)
              then pure (Just i)
              else min'' (i+1) $ case mb of
                Nothing      -> Just (i, ss ri)
                Just (b, bc) -> Just $ if ss ri < bc
                  then (i, ss ri)
                  else (b, bc)

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
  gmips term = case term of
    Br dest -> tell1 $ J dest

    IfBr cond trueDest falseDest -> do
      x <- getReg1 term
      tell
        [ Bne x Zero trueDest
        , J falseDest ]

    CondBr rel op0 op1 trueDest falseDest -> do
      (x, y) <- getReg2 term

      case rel of
        LTF -> undefined -- c.lt.s $f1, $f2 ; bc1t l`

        LEF -> undefined -- c.le.s $f1, $f2 ; bc1t l`

        GTF -> undefined -- c.lt.s $f2, $f1 ; bc1t l`

        GEF -> undefined -- c.le.s $f2, $f1 ; bc1t l`

        EQF -> undefined -- c.eq.s $f1, $f2 ; bc1t l`

        NEF -> undefined -- c.eq.s $f1, $f2 ; bc1f l`

        LTI -> do
          tell1 $ Slt (Scratch 0) x y
          tell1 $ Bne (Scratch 0) Zero trueDest

        LEI -> do
          tell1 $ Slt (Scratch 0) y x
          tell1 $ Beq (Scratch 0) Zero trueDest

        GTI -> do
          tell1 $ Slt (Scratch 0) y x
          tell1 $ Bne (Scratch 0) Zero trueDest

        GEI -> do
          tell1 $ Slt (Scratch 0) x y
          tell1 $ Beq (Scratch 0) Zero trueDest

        EQI ->
          tell1 $ Beq x y trueDest

        NEI ->
          tell1 $ Bne x y trueDest

        FAI -> do
          tell1 $ BinOp RemI (Scratch 0) y x
          tell1 $ Beq (Scratch 0) Zero trueDest

        NFI -> do
          tell1 $ BinOp RemI (Scratch 0) y x
          tell1 $ Bne (Scratch 0) Zero trueDest

      tell1 $ J falseDest

    Return -> tell
      [ LoadW RA (4, FP)
      , Jr RA ]

    Exit -> tell
      [ LoadI (Scratch 0) (IC 10)
      , Syscall ]

instance Gmips TAC where
  gmips tac = case tac of
    TAC.Comment str -> tell1 $ Comment str

    Var _ name offs _ ->

      home %= Map.insert (R name) offs

    op := operation -> case operation of
      B o l r
        | intconst l && intconst r -> do
          traceM $ "forgot to fold " <> emit tac <> ", :("
          x <- getReg1 tac
          tell
            [ LoadI  x   (extract l)
            , BinOpi o x x (extract r) ]
        | intconst r -> do
          (x, y) <- getReg2 tac
          tell1 $ BinOpi o x y (extract r)
        | intconst l -> if commutative o
          then gmips (op := B o r l)
          else do
            (x, y) <- getReg2 tac
            tell
              [ LoadI (Scratch 0) (extract l)
              , BinOp o x (Scratch 0) y ]
        | otherwise -> do
          (x, y, z) <- getReg3 tac
          tell1 $ BinOp o x y z

        where
          intconst = \case
            C (FC _) -> False
            C _ -> True
            _ -> False
          extract = \case
            C c -> c
            _   -> internal "Attempted to extract from non-constant"
          commutative = (`elem` ([ AddI, MulI, BAnd, BOr, BXor ] :: [BOp]))

      U o u -> do
        (x, y) <- getReg2 tac
        case o of
          NegF -> undefined -- jeje que es flout?
          NegI -> tell1 $ BinOp SubI x Zero y
          BNot -> tell1 $ BinOp BXor x Zero y
          Id   -> pure ()

    _ :=# (_, C (IC n)) -> do
      (x, y) <- getReg2 tac
      tell1 $ LoadW x (n, y)

    _ :=# (_, _) -> do
      (x, y, z) <- getReg3 tac
      tell
        [ BinOp AddI (Scratch 0) y z
        , LoadW x (0, (Scratch 0)) ]

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

    _ :=& o@(R name) -> do
      x <- getReg1 tac
      h <- use home
      tell1 $ case o `Map.lookup` h of
        Nothing     -> LoadA x name
        Just offset -> BinOpi AddI x FP (IC offset)

    _ :=& _ -> internal $ "Invalid address-of"

    _ :=@ (r@(R name), C (IC n)) -> do
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

    _ :=@ (C _, _) -> internal "_ :=@ (Constant, _)"

    Param (C c) -> case c of
      FC _ -> undefined
      _    -> tell
        [ LoadI (Scratch 0) c
        , BinOpi AddI SP SP (IC $ -4)
        , StoreW (Scratch 0) (0, SP) ]

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

    RefParam _ -> internal "RefParam (Const/Temp)"

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
      tell1 $ Move SP FP


    Answer (C c) -> case c of
      FC _ -> undefined
      _    -> tell
        [ LoadI (Scratch 0) c
        , StoreW (Scratch 0) (8, FP) ]

    Answer _ -> do
      x <- getReg1 tac
      tell1 $ StoreW x (8, FP)

    -- c -> tell1 $ Comment (emit c)
