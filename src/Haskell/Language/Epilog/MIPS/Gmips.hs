{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections    #-}

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
import           Control.Lens               (at, use, (%=), (.=), (<-=), (?=))
import           Data.Array.MArray          (readArray, writeArray)
import           Data.List                  ((\\))
import qualified Data.Map                   as Map (empty, insert, lookup, size)
import           Data.Maybe                 (fromMaybe)
--------------------------------------------------------------------------------
import           Debug.Trace                (traceM)
--------------------------------------------------------------------------------

data Except a = Except a

except :: a -> Except a
except =  Except

--------------------------------------------------------------------------------

data TAC'
  = IT TAC
  | TT Terminator

instance Emit TAC' where
  emit = \case
    IT i -> emit i
    TT t -> emit t

class GetReg i where
  getReg1 :: i -> MIPSMonad  Register
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

--------------------------------------------------------------------------------

class Gmips a where
  gmips :: a -> MIPSMonad ()

--------------------------------------------------------------------------------

getReg3' :: TAC' -> MIPSMonad (Register, Register, Register)
getReg3' tac = do
  vd <- use variables
  let
    (writing, op1, op2, op3) = case tac of
      IT (a := B _o b c) -> (True , a, b, c)
      IT (a :=#  (b, c)) -> (True , a, b, c)
      IT ((a, b)  :#= c) -> (False, a, b, c)

      _ -> internal $ emit tac

  if| op1 == op2 && op1 == op3 -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg op1
          load x op1
          pure x

        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)
      pure (x, x, x)

    | op1 == op2 -> do
      y <- case op3 `Map.lookup` vd of
        Nothing -> do
          y <- case op1 `Map.lookup` vd of
            Nothing -> freshReg  op3
            Just x  -> freshReg' op3 (except [x])
          load y op3
          pure y

        Just y -> pure y

      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg' op1 (except [y])
          load x op1
          pure x
        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)
      pure (x, x, y)

    | op1 == op3 -> do
      y <- case op2 `Map.lookup` vd of
        Nothing -> do
          y <- case op1 `Map.lookup` vd of
            Nothing -> freshReg  op2
            Just x  -> freshReg' op2 (except [x])
          load y op2
          pure y

        Just y -> pure y

      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg' op1 (except [y])
          load x op1
          pure x
        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)
      pure (x, y, x)

    | otherwise {- op2 == op3 || All different -} -> do
      (y, z) <- if op2 == op3
        then case op2 `Map.lookup` vd of
          Nothing -> do
            y <- case op1 `Map.lookup` vd of
              Nothing -> freshReg  op2
              Just x  -> freshReg' op2 (except [x])
            load y op2
            pure (y, y)

          Just y -> pure (y, y)

        else {- op2 /= op3 -} do
          y <- case op2 `Map.lookup` vd of
            Nothing -> do
              y <- case (op1 `Map.lookup` vd, op3 `Map.lookup` vd) of
                (Nothing, Nothing) -> freshReg  op2
                (Just x , Nothing) -> freshReg' op2 (except [x])
                (Nothing, Just z ) -> freshReg' op2 (except [z])
                (Just x , Just z ) -> freshReg' op2 (except [x, z])
              load y op2
              pure y

            Just y -> pure y

          z <- case op3 `Map.lookup` vd of
            Nothing -> do
              z <- case op1 `Map.lookup` vd of
                Nothing -> freshReg' op3 (except [y])
                Just x  -> freshReg' op3 (except [x, y])
              load z op3
              pure z

            Just z -> pure z

          pure (y, z)

      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg' op1 (except [y, z])
          if writing
            then op1 `nowAt` x
            else load x op1
          pure x

        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)

      pure (x, y, z)


getReg2' :: TAC' -> MIPSMonad (Register, Register)
getReg2' tac = do
  vd <- use variables
  let
    (writing, op1, op2) = case tac of
      IT (a := B _o b (C _))  -> (True , a, b)
      IT (a := B _o (C _) b)  -> (True , a, b)
      IT (a := U _o b    )    -> (True , a, b)
      IT (a :=@ (R  _, b))    -> (True , a, b)
      IT (a :=@ (RF _, b))    -> (True , a, b)

      IT (a :=# (b@(T _), C _)) -> (True , a, b)
      IT (a :=# (R _    , b))   -> (True , a, b)
      IT (_ :=# (C _    , _))   -> internal "Constant base in array access"
      
      IT ((a  , C _) :#= b  ) -> (False, a, b)
      IT ((a  , b)   :#= C _) -> (False, a, b)
      IT ((R _, a)   :#= b  ) -> (False, a, b)

      IT (a :=* b       )     -> (True , a, b)
      IT (a :*= b       )     -> (False, a, b)

      TT (CondBr _ a b _ _)   -> (False, a, b)

      _ -> internal $ emit tac

  if op1 == op2
    then do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg op1
          load x op1
          pure x

        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)
      pure (x, x)

    else {- op1 /= op2 -} do
      y <- case op2 `Map.lookup` vd of
        Nothing -> do
          y <- case op1 `Map.lookup` vd of
            Nothing -> freshReg  op2
            Just x  -> freshReg' op2 (except [x])
          load y op2
          pure y

        Just y -> pure y

      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg' op1 (except [y])
          if writing
            then op1 `nowAt` x
            else load x op1
          pure x

        Just x -> do
          when writing $ spill' x (except op1)
          pure x

      when writing $ x `nowHas` (Dirty, op1)
      pure (x, y)


getReg1' :: TAC' -> MIPSMonad Register
getReg1' tac = do
  vd <- use variables
  let
    (writing, op) = case tac of
      IT (a := U Id (C _))       -> (True , a)
      IT (a := B _o (C _) (C _)) -> (True , a)
      IT (a := U _o (C _))       -> (True , a)

      IT (a :=@ (R  _,C _))      -> (True , a)
      IT (a :=@ (RF _,C _))      -> (True , a)
      IT (a :=& R _name   )      -> (True , a)
      IT (a :<- _         )      -> (True , a)

      IT (a :=# (R _, C _))      -> (True , a)

      IT ((a  , C _) :#= C _)    -> (False, a)
      IT ((R _, a  ) :#= C _)    -> (False, a)
      IT ((R _, C _) :#= a  )    -> (False, a)
      IT (a :*= C _       )      -> (False, a)

      IT (_ := U Id     a)       -> (False, a)
      IT (Param (C _)   )        -> internal "Constant Param"
      IT (Param a       )        -> (False, a)
      IT (Answer (C _)  )        -> internal "Constant Answer"
      IT (Answer a      )        -> (False, a)
      TT (IfBr a _ _)            -> (False, a)
      TT (CondBr _ a (C _) _ _)  -> (False, a)
      TT (CondBr _ (C _) a _ _)  -> (False, a)

      _ -> internal $ emit tac

  x <- case op `Map.lookup` vd of
    Nothing -> do
      x <- freshReg op
      if writing
        then op `nowAt` x -- No need to spill here, it's guaranteed fresh.
        else load x op
      pure x

    Just x -> do
      when writing $ spill' x (except op)
      pure x

  when writing $ x `nowHas` (Dirty, op)
  pure x


load :: Register -> Operand -> MIPSMonad ()
load r op = do
  h <- use home
  case op `Map.lookup` h of
    Nothing -> tell1 $ case op of
      RF name -> LoadFG r (name, 0)
      R name  -> LoadWG r (name, 0)
      _ -> internal $
        "Loading Constant or unavailable Temp " <> emit op

    Just offset ->
      tell1 $ (if isFloatReg r then LoadF else LoadW) r (offset, FP)

  r `nowHas` (Clean, op)
  op `nowAt` r


cost :: Operand -> MIPSMonad Word32
cost op = do
  h <- use home
  pure $ if temporary op && isNothing (op `Map.lookup` h)
    then 10
    else 1

  where
    temporary :: Operand -> Bool
    temporary = \case
      T  _ -> True
      TF _ -> True
      _ -> False


isFloatOp :: Operand -> Bool
isFloatOp = \case
  RF _ -> True
  TF _ -> True
  _    -> False


freshReg :: Operand -> MIPSMonad Register
freshReg op = freshReg' op (except [])


freshReg' :: Operand -> Except [Register] -> MIPSMonad Register
freshReg' op (Except exceptions) = do
  r <- fromMaybe msg <$> min'
  spill r

  pure r

  where
    msg = internal $
      "couldn't find a fresh reg meeting exceptions " <> show exceptions

    min' :: MIPSMonad (Maybe Register)
    min' = fmap (if isFloatOp op then mkFloatR else mkGeneral) <$>
      min'' first Nothing

    first, last' :: Word32
    (first, last') = if isFloatOp op
      then (nGeneralRegs, nTotalRegs)
      else (0, nGeneralRegs)

    min'' :: Word32 -> Maybe (Word32, Word32) -> MIPSMonad (Maybe Word32)
    min'' i mb | i == last' = case mb of
      (Just (b, _bc)) -> pure . Just $ b
      Nothing         -> pure Nothing
    min'' i  mb = if i `elem` (num <$> exceptions)
      then min'' (succ i) mb
      else do
        regs <- use registers
        ri <- liftIO $ readArray regs i
        if not (dirty ri) || ss ri == 0
          then pure (Just i)
          else min'' (i+1) $ case mb of
            Nothing      -> Just (i, ss ri)
            Just (b, bc) -> Just $ if ss ri < bc
              then (i, ss ri)
              else (b, bc)

instance Gmips Program where
  gmips Program { datas, modules } = do
    unless (null datas) $ do
      tell1 $ DataSection
      mapM_ gmips datas

    unless (null modules) $ do
      tell1 TextSection
      tell1 $ Global "main"
      mapM_ gmips modules

instance Gmips Data where
  gmips VarData { dName, dSpace } = tell
    [ Align
    , Data dName dSpace ]
  gmips StringData { dName, dString } = tell1 $ MString dName dString

instance Gmips Module where
  gmips Module { mName, mBlocks } = do
    tell1 $ Comment mName
    mapM_ gmips mBlocks

    h    <-  use home
    ssq  <|= fromIntegral (Map.size h * 4)
    home  .= Map.empty

instance Gmips Block where
  gmips Block { lbl, tacs, term } = do
    tell1 $ MLabel lbl
    mapM_ gmips tacs
    spillAll
    gmips term
    resetDescs

instance Gmips Terminator where
  gmips term = tell1 (Comment $ "  " <> show term) >> case term of
    Br dest -> tell1 $ J dest

    IfBr _cond trueDest falseDest -> do
      x <- getReg1 term
      tell
        [ Bne x Zero trueDest
        , J falseDest ]

    CondBr rel op0 op1 trueDest falseDest -> do
      let
        (scratch, load') = if isFloatOp op0
          then (ScratchF, LoadFI)
          else (Scratch , LoadI)
      (x, y) <- case (op0, op1) of
        (C c0, C c1) -> do
          traceM $ "forgot to fold " <> emit term <> ", :("
          tell
            [ load' (scratch 0) c0
            , load' (scratch 1) c1 ]
          pure (scratch 0, scratch 1)

        (   _, C c1) -> do
          tell1 $ load' (scratch 0) c1
          x <- getReg1 term
          pure (x, scratch 0)

        (C c0, _   ) -> do
          tell1 $ load' (scratch 0) c0
          y <- getReg1 term
          pure (scratch 0, y)

        _            -> getReg2 term

      case rel of
        LTF -> tell
          [ Clts x y
          , Bc1t trueDest ]

        LEF -> tell
          [ Cles x y
          , Bc1t trueDest ]

        GTF -> tell
          [ Clts y x
          , Bc1t trueDest ]

        GEF -> tell
          [ Cles y x
          , Bc1t trueDest ]

        EQF -> tell
          [ Ceqs x y
          , Bc1t trueDest ]

        NEF -> tell
          [ Ceqs x y
          , Bc1f trueDest ]

        LTI -> tell
          [ Slt (Scratch 0) x y
          , Bne (Scratch 0) Zero trueDest ]

        LEI -> tell
          [ Slt (Scratch 0) y x
          , Beq (Scratch 0) Zero trueDest ]

        GTI -> tell
          [ Slt (Scratch 0) y x
          , Bne (Scratch 0) Zero trueDest ]

        GEI -> tell
          [ Slt (Scratch 0) x y
          , Beq (Scratch 0) Zero trueDest ]

        EQI ->
          tell1 $ Beq x y trueDest

        NEI ->
          tell1 $ Bne x y trueDest

        FAI -> tell
          [ BinOp RemI (Scratch 0) y x
          , Beq (Scratch 0) Zero trueDest ]

        NFI -> tell
          [ BinOp RemI (Scratch 0) y x
          , Bne (Scratch 0) Zero trueDest ]

      tell1 $ J falseDest

    Return -> tell
      [ LoadW RA (4, FP)
      , Jr RA ]

    Exit -> tell
      [ LoadI (Scratch 0) (IC 10)
      , Syscall ]

instance Gmips TAC where
  gmips tac = tell1 (Comment $ "  " <> emit tac) >> case tac of
    TAC.Comment str -> tell1 $ Comment str

    Var      name offs _ -> home %= Map.insert (R  name) offs
    RefVar   name offs _ -> home %= Map.insert (R  name) offs
    FloatVar name offs _ -> home %= Map.insert (RF name) offs

    op := operation -> case operation of
      B o l r
        | intconst l && intconst r -> do
          traceM $ "forgot to fold " <> emit tac <> ", :("
          x <- getReg1 tac
          tell
            [ LoadI  x     (extract l)
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
        | floatconst r -> do
          (x, y) <- getReg2 tac
          tell
            [ LoadFI (ScratchF 0) (extract r)
            , BinOp o x y (ScratchF 0) ]
        | floatconst l -> do
          (x, y) <- getReg2 tac
          tell
            [ LoadFI (ScratchF 0) (extract l)
            , BinOp o x (ScratchF 0) y ]
        | otherwise -> do
          (x, y, z) <- getReg3 tac
          tell1 $ BinOp o x y z

        where
          floatconst = \case
            C (FC _ ) -> True
            _         -> False
          intconst = \case
            C (FC _) -> False
            C _ -> True
            _ -> False
          extract = \case
            C c -> c
            _   -> internal "Attempted to extract from non-constant"
          commutative = (`elem` ([ AddI, MulI, BAnd, BOr, BXor ] :: [BOp]))

      U o (C c) -> do
        x <- getReg1 tac
        case o of
          NegF -> tell
            [ LoadFI (ScratchF 0) (FC 0.0)
            , BinOpi SubF x (ScratchF 0) c ]
          NegI -> tell1 $ BinOpi SubI x Zero c
          BNot -> tell1 $ BinOpi BXor x Zero c
          ItoF -> tell1 $ I2Fi x c
          FtoI -> tell1 $ F2Ii x c
          Id   -> tell1 $ case c of
            FC {} -> LoadFI x c
            _ -> LoadI  x c

      U Id _u -> do
        x <- getReg1 tac

        vd <- use variables
        case op `Map.lookup` vd of
          Just x' | x' == x -> pure ()

          Just x' {- x' /= x -} -> do
            regs <- use registers
            ssx <- cost op
            liftIO $ do
              rd'@RegDesc { values, ss } <- readArray regs (num x')
              writeArray regs (num x') rd'
                { values = values \\ [op]
                , ss = ss - ssx }

              rd@RegDesc { values, ss } <- readArray regs (num x)
              writeArray regs (num x) rd
                { values = op : values
                , ss = ss + ssx
                , dirtyness = Dirty }
            op `nowAt` x

          Nothing -> do
            regs <- use registers
            ssx <- cost op
            liftIO $ do
              rd@RegDesc { values, ss } <- readArray regs (num x)
              writeArray regs (num x) rd
                { values = op : values
                , ss = ss + ssx
                , dirtyness = Dirty }
            op `nowAt` x

      U o _u -> do
        (x, y) <- getReg2 tac

        case o of
          NegF -> tell
            [ LoadFI (ScratchF 0) (FC 0.0)
            , BinOp SubF x (ScratchF 0) y ]
          NegI -> tell1 $ BinOp SubI x Zero y
          BNot -> tell1 $ BinOp BXor x Zero y
          ItoF -> tell1 $ I2F x y
          FtoI -> tell1 $ F2I x y
          Id   -> internal "Id is a special case"

    op :=# (b, C (IC n)) -> case b of
      R name -> do
        h <- use home
        x <- getReg1 tac

        case b `Map.lookup` h of
          -- Global
          Nothing -> tell1 $ if isFloatOp op
            then LoadFG x (name, n)
            else LoadWG x (name, n)

          -- Local
          Just off -> tell1 $ if isFloatOp op
            then LoadF x (n + off, FP)
            else LoadW x (n + off, FP)

      t@T{} -> do
        (x, y) <- getReg2 tac
        tell1 $ if isFloatOp op
          then LoadF x (n, y)
          else LoadW x (n, y)

      _ -> internal $ "impossible base in :=#, " <> emit b

    op :=# (b, _) -> case b of
      R name -> do
        h <- use home
        (x, y) <- getReg2 tac
        case b `Map.lookup` h of
          -- Global
          Nothing -> tell1 $ if isFloatOp op
            then LoadFGR x (name, y)
            else LoadWGR x (name, y)

          -- Local
          Just off -> tell
            [ BinOpi AddI (Scratch 0) FP (IC off)
            , BinOp  AddI (Scratch 0) (Scratch 0) y
            , if isFloatOp op
                then LoadF x (0, Scratch 0)
                else LoadW x (0, Scratch 0) ]

      t@T{} -> do
        (x, y, z) <- getReg3 tac
        tell
          [ BinOp AddI (Scratch 0) y z
          , if isFloatOp op
              then LoadF x (0, Scratch 0)
              else LoadW x (0, Scratch 0) ]

      _ -> internal $ "impossible base in :=#, " <> emit b

    (b, C (IC n)) :#= C v -> do
      scratch <- case v of
        FC {} -> do
          tell1 $ LoadFI (ScratchF 0) v
          pure (ScratchF 0)
        _  -> do
          tell1 $ LoadI  (Scratch  0) v
          pure (Scratch 0)

      case b of
        R name -> do
          spillAll
          resetDescs
          h <- use home
          case b `Map.lookup` h of
            -- Global
            Nothing -> tell1 $ case v of
              FC {} -> StoreFG scratch (name, n)
              _     -> StoreWG scratch (name, n)

            -- Local
            Just off -> tell1 $ case v of
              FC {} -> StoreW scratch (n + off, FP)
              _     -> StoreF scratch (n + off, FP)

        t@T{} -> do
          x <- getReg1 tac
          spillAll
          resetDescs
          tell1 $ case v of
            FC {} -> StoreF scratch (n, x)
            _     -> StoreW scratch (n, x)

    (b, C (IC n)) :#= op -> case b of
      R name -> do
        h <- use home
        x <- getReg1 tac
        spillAll
        resetDescs
        case b `Map.lookup` h of
          -- Global
          Nothing -> tell1 $ if isFloatOp op
            then StoreFG x (name, n)
            else StoreWG x (name, n)

          -- Local
          Just off -> do
            tell1 $ if isFloatOp op
              then StoreF x (n + off, FP)
              else StoreW x (n + off, FP)

      t@T{} -> do
        (x, y) <- getReg2 tac
        spillAll
        resetDescs
        tell1 $ if isFloatOp op
          then StoreF y (n, x)
          else StoreW y (n, x)

      _ -> internal $ "impossible base in :#=, " <> emit b

    (b, _) :#= C v -> do
      scratch <- case v of
        FC {} -> do
          tell1 $ LoadFI (ScratchF 0) v
          pure (ScratchF 0)
        _  -> do
          tell1 $ LoadI  (Scratch  0) v
          pure (Scratch 0)

      case b of
        R name -> do
          h <- use home
          x <- getReg1 tac
          case b `Map.lookup` h of
            -- Global
            Nothing -> tell1 $ case v of
              FC {} -> StoreFGR scratch (name, x)
              _     -> StoreWGR scratch (name, x)

            -- Local
            Just off -> tell
              [ BinOpi AddI (Scratch 1) FP (IC off)
              , BinOp  AddI (Scratch 1) (Scratch 1) x
              , case v of
                  FC {} -> StoreF scratch (0, Scratch 1)
                  _     -> StoreW scratch (0, Scratch 1) ]

        t@T{} -> do
          (x, y) <- getReg2 tac
          spillAll
          resetDescs
          tell
            [ BinOp AddI (Scratch 1) x y
            , case v of
                FC {} -> StoreF scratch (0, Scratch 1)
                _     -> StoreW scratch (0, Scratch 1) ]

    (b, _) :#= op -> case b of
      R name -> do
        h <- use home
        (x, y) <- getReg2 tac
        case b `Map.lookup` h of
          -- Global
          Nothing -> tell1 $ if isFloatOp op
            then StoreFGR y (name, x)
            else StoreWGR y (name, x)

          -- Local
          Just off -> tell
            [ BinOpi AddI (Scratch 0) FP (IC off)
            , BinOp  AddI (Scratch 0) (Scratch 0) x
            , if isFloatOp op
                then StoreF y (0, Scratch 0)
                else StoreW y (0, Scratch 0) ]

      t@T{} -> do
        (x, y, z) <- getReg3 tac
        spillAll
        resetDescs
        tell
          [ BinOp AddI (Scratch 0) x y
          , if isFloatOp op
              then StoreF z (0, Scratch 0)
              else StoreW z (0, Scratch 0) ]

    _ :=* C _ -> internal "Const dereference"

    op :=* _ -> do
      (x, y) <- getReg2 tac
      tell1 $ if isFloatOp op
        then LoadF x (0, y)
        else LoadW x (0, y)

    C _ :*= _ -> internal "Const ptr assign"

    _ :*= C v -> do
      (store, scratch) <- case v of
        FC {} -> do
          tell1 $ LoadFI (ScratchF 1) v
          pure (StoreF, ScratchF 1)
        _  -> do
          tell1 $ LoadI  (Scratch  1) v
          pure (StoreW, Scratch 1)

      x <- getReg1 tac
      spillAll
      resetDescs
      tell1 $ store scratch (0, x)

    _ :*= op -> do
      (x, y) <- getReg2 tac
      spillAll
      resetDescs
      tell1 $ if isFloatOp op
        then StoreF y (0, x)
        else StoreW y (0, x)

    _ :=& o@(R name) -> do
      x <- getReg1 tac
      h <- use home
      tell1 $ case o `Map.lookup` h of
        Nothing     -> LoadA x name
        Just offset -> BinOpi AddI x FP (IC offset)

    _ :=& o@(RF name) -> do
      x <- getReg1 tac
      h <- use home
      tell1 $ case o `Map.lookup` h of
        Nothing     -> LoadA x name
        Just offset -> BinOpi AddI x FP (IC offset)

    _ :=& _ -> internal "Invalid address-of"

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

    _ :=@ (r@(RF name), C (IC n)) -> do
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

    _ :=@ (r@(RF name), _) -> do
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

    a :=@ (b@(T _), c) -> gmips (a := B AddI b c)

    _ :=@ (TF _, _) -> internal "_ :=@ (Float _, _) is absurd"

    _ :=@ (C _, _) -> internal "_ :=@ (Constant, _) is impossible"

    Param (C c) -> case c of
      FC _ -> tell
        [ LoadFI (ScratchF 0) c
        , BinOpi AddI SP SP (IC $ -4)
        , StoreF (ScratchF 0) (0, SP) ]
      _    -> tell
        [ LoadI (Scratch 0) c
        , BinOpi AddI SP SP (IC $ -4)
        , StoreW (Scratch 0) (0, SP) ]

    Param op -> do
      fstp <- use fstParam
      when fstp $ do
        spillAll
        fstParam .= False

      x <- getReg1 tac

      tell1 $ BinOpi AddI SP SP (IC $ -4)
      tell1 $ if isFloatOp op
        then StoreF x (0, SP)
        else StoreW x (0, SP)

    RefParam o@(R name) -> do
      fstp <- use fstParam
      when fstp $ do
        spillAll
        fstParam .= False
      h <- use home
      tell
        [ BinOpi AddI SP SP (IC $ -4)
        , case o `Map.lookup` h of
            Nothing     -> LoadA (Scratch 0) name
            Just offset -> BinOpi AddI (Scratch 0) FP (IC offset)
        , StoreW (Scratch 0) (0, SP) ]

    RefParam _ -> internal "RefParam (Const/Temp)"

    Call proc -> do
      -- Some procedures have no parameters!
      fstp <- use fstParam
      when fstp spillAll
      resetDescs
      fstParam .= True
      tell
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
      -- Some procedures have no parameters!
      fstp <- use fstParam
      when fstp spillAll
      x <- getReg1 tac
      resetDescs
      op `nowAt` x
      x `nowHas` (Dirty, op)
      fstParam .= True
      tell
        [ BinOpi SubI SP SP (IC 12)
        , StoreW FP (0, SP)
        , Jal $ "proc_" <> proc
        -- prolog
        -- ???
        -- profit
        -- epilog
        , (if isFloatReg x then LoadF else LoadW) x (8, FP)
        -- Cleanup
        ]

    Cleanup n -> tell
      [ LoadW FP (0, FP)
      , BinOpi AddI SP SP (IC . fromIntegral $ n+12) ]

    Prolog n -> do
      tell1 MIPSProlog
      vsp .= fromIntegral (-n)

    Epilog _ -> tell1 $ Move SP FP

    Answer (C c) -> case c of
      FC _ -> tell
        [ LoadFI (ScratchF 0) c
        , StoreF (ScratchF 0) (8, FP) ]
      _    -> tell
        [ LoadI  (Scratch 0) c
        , StoreW (Scratch 0) (8, FP) ]

    Answer _ -> do
      x <- getReg1 tac
      tell1 $ if isFloatReg x
        then StoreF x (8, FP)
        else StoreW x (8, FP)

    -- c -> tell1 $ Comment (emit c)

-- `spillAll` spills all variables in all registers.
spillAll :: MIPSMonad ()
spillAll = mapM_ spill allRegisters
  where
    allRegisters :: [Register]
    allRegisters = mkRegister <$> [0..nTotalRegs-1]

-- `spill r op` spills all variables in register `r`.
spill :: Register -> MIPSMonad ()
spill r = spill'' r (except Nothing)

-- `spill' r op` spills all variables in register `r` except `op`.
spill' :: Register
       -> Except Operand
       -> MIPSMonad ()
spill' r (Except op) = spill'' r (except (Just op))

-- `spill'' r ops` spills all variables in register `r` except those in `ops`.
spill'' :: Foldable f
        => Register
        -> Except (f Operand)
        -> MIPSMonad ()
spill'' r (Except ops) = do
  regs <- use registers
  h <- use home
  rd@RegDesc { values } <- liftIO $ readArray regs (num r)
  when (dirty rd) . forM_ values $ \v -> when (v `notElem` ops) $
    case v `Map.lookup` h of
      Nothing     -> case v of
        (R  name) -> tell1 $ StoreWG r (name, 0)
        (RF name) -> tell1 $ StoreFG r (name, 0)

        temp@(T _)  -> do
          toffs <- vsp <-= 4
          tell1 $ StoreW r (toffs, FP)
          home %= Map.insert temp toffs

        temp@(TF _)  -> do
          toffs <- vsp <-= 4
          tell1 $ StoreF r (toffs, FP)
          home %= Map.insert temp toffs

        _ -> internal $
          "trying to spill a constant operand" <> emit v

      Just offset ->
        tell1 $ (if isFloatReg r then StoreF else StoreW) r (offset, FP)

nowAt :: Operand -> Register -> MIPSMonad ()
op `nowAt` r = variables . at op ?= r

-- nowHas :: Register -> (Dirtyness, op) -> MIPSMonad ()
-- r `nowHas` (dirty, op) = do
--   regs <- use registers
--   ss <- cost op
--   liftIO $ writeArray regs (num r) (RegDesc [op] ss dirty)

nowHas :: Register -> (Dirtyness, Operand) -> MIPSMonad ()
r `nowHas` (d, op) = do
  regs <- use registers

  RegDesc { values } <- liftIO $ readArray regs (num r)

  forM_ values $ \v -> when (v /= op) $
    variables . at v .= Nothing

  ss <- cost op
  liftIO $ writeArray regs (num r) (RegDesc [op] ss d)
