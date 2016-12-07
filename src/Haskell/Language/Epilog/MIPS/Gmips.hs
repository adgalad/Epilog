{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections   #-}

module Language.Epilog.MIPS.Gmips
  ( Gmips (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.IR.TAC     hiding (Comment)
import qualified Language.Epilog.IR.TAC     as TAC (TAC (Comment))
import           Language.Epilog.MIPS.MIPS
import           Language.Epilog.MIPS.Monad
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens               (use, (%=), (.=), (<-=))
import           Data.Array.IO              (IOArray)
import           Data.Array.MArray          (readArray, writeArray)
import qualified Data.Map                   as Map (empty, insert, lookup, delete)
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
  let
    (op1, op2, op3) = case t of
      IT (a := B o b c) -> (a, b, c)
      IT (a :=# (b, c)) -> (a, b, c)
      -- (_, _) :#= _ -> (a, b, c) -- FIXME!!!
  rd1 <- use $ if isFloat op1 then floatregs else registers
  rd2 <- use $ if isFloat op2 then floatregs else registers
  rd3 <- use $ if isFloat op2 then floatregs else registers
  ss1 <- cost op1
  ss2 <- cost op2
  ss3 <- cost op3
  if
    | op1 == op2 && op1 == op3 -> do
        x <- case op1 `Map.lookup` vd of
          Nothing -> do
            x <- freshReg op1
            load x op1
            variables %= Map.insert op1 x
            pure x
          Just x -> pure x

        liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)

        pure (x, x, x)

    | op1 == op2 -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do 
          x <- freshReg op1
          variables %= Map.insert op1 x
          pure x
        Just x  -> pure x
      
      liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)
      
      y <- case op3 `Map.lookup` vd of
        Nothing -> do
          y <- freshReg' [num x] op3
          load y op3
          variables %= Map.insert op3 y
          liftIO $ writeArray rd3 (num y) (RegDesc [op3] ss3 True)
          pure y
        Just y -> pure y

      pure (x, x, y)

    | op1 == op3 -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do 
          x <- freshReg op1
          variables %= Map.insert op1 x
          pure x
        Just x  -> pure x
      liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)
      
      y <- case op2 `Map.lookup` vd of
        Nothing -> do
          y <- freshReg' [num x] op2
          load y op2
          variables %= Map.insert op2 y
          liftIO $ writeArray rd2 (num y) (RegDesc [op2] ss2 True)
          pure y
        Just y -> pure y
      
      
      
      pure (x, y, x)

    | otherwise -> do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do 
          x <- freshReg op1
          variables %= Map.insert op1 x
          pure x
        Just x  -> pure x
      liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)
      
      (y, z) <- if op2 == op3
        then do
          y <- case op2 `Map.lookup` vd of
            Nothing -> do
              y <- freshReg' [num x] op2
              load y op2
              variables %= Map.insert op2 y
              liftIO $ writeArray rd2 (num y) (RegDesc [op2] ss2 True)
              pure y
            Just y -> pure y
          
          pure (y, y)
        
        else do
          y <- case op2 `Map.lookup` vd of
            Nothing -> do
              y <- freshReg' [num x] op2
              load y op2
              variables %= Map.insert op2 y
              liftIO $ writeArray rd2 (num y) (RegDesc [op2] ss2 True)
              pure y
            Just y -> pure y
          
          z <- do 
            case op3 `Map.lookup` vd of
              Nothing -> do
                z <- freshReg' (num <$> [x, y]) op3
                load z op3
                variables %= Map.insert op3 z
                liftIO $ writeArray rd3 (num z) (RegDesc [op3] ss3 True)
                pure z
              Just z -> pure z
          pure (y, z)

      
      pure (x, y, z)

getReg2' :: TAC' -> MIPSMonad (Register, Register)
getReg2' t = do
  vd <- use variables
  h  <- use home
  
  let
    (op1, op2) = case t of
      IT (a := B o b (C _))  -> (a, b)
      IT (a := B o (C _) b)  -> (a, b)
      IT (a := U o b    )    -> (a, b)
      IT (a :=@ ((R  _), b)) -> (a, b)
      IT (a :=@ ((RF _), b)) -> (a, b)
      IT (a :=# (b, C _))    -> (a, b)
      IT (a :=* b       )    -> (a, b)
      -- IT ((a, C _) :#= b)  -> (a, b) -- FIXME!!!
      -- IT (a :*= b       )  -> (a, b) -- FIXME!!!
      TT (CondBr _ a b _ _)  -> (a, b)
      IT t -> internal $ emit t
      TT t -> internal $ emit t

  rd1 <- use $ if isFloat op1 then floatregs else registers
  rd2 <- use $ if isFloat op2 then floatregs else registers
  ss1 <- cost op1
  ss2 <- cost op2

  if op1 == op2
    then do
      x <- case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg op1
          load x op1
          variables %= Map.insert op1 x
          pure x
        Just x -> pure x
      pure (x, x)

    else do
      case op1 `Map.lookup` vd of
        Nothing -> do
          x <- freshReg op1
          variables %= Map.insert op1 x
          liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)
          case op2 `Map.lookup` vd of
            Just y -> do
              pure (x, y)
            Nothing -> do
              load x op2
              pure (x, x)

        Just x -> do
          liftIO $ writeArray rd1 (num x) (RegDesc [op1] ss1 True)
          case op2 `Map.lookup` vd of
            Just y  -> pure (x, y)
            Nothing -> do
              y <- freshReg' [num x] op2 -- spill x op1
              load y op2
              variables %= Map.insert op2 y
              liftIO $ writeArray rd2 (num y) (RegDesc [op2] ss2 True)
              pure (x, y)

getReg1' :: TAC' -> MIPSMonad Register
getReg1' t = do
  vd <- use variables
  h  <- use home
  
  let
    op = case t of
      -- Special case (value)
      IT (x := B o (C _) (C _)) -> x
      IT (x := U o (C _)) -> x
      IT (_ := U Id x)    -> x
      
      IT (Param (C _)   ) -> internal "Constant Param"
      IT (Param x       ) -> x
      IT (Answer (C _)  ) -> internal "Constant Answer"
      IT (Answer x      ) -> x

      IT (x :=@ (R  _,C _)) -> x
      IT (x :=@ (RF _,C _)) -> x
      IT (x :=& R name  ) -> x
      IT (x :<- _       ) -> x

      TT (IfBr x _ _)     -> x
      TT (CondBr _ x (C _) _ _) -> x
      TT (CondBr _ (C _) x _ _) -> x

  rd <- use $ if isFloat op then floatregs else registers
  ss' <- cost op

  x <- case op `Map.lookup` vd of
    Nothing -> do
      x <- freshReg op
      variables %= Map.insert op x
      when (t `reading` op) $ load x op
      pure x

    Just x -> do
      pure x
  
  case t of
    IT (Param  _ )   -> pure ()
    IT (Answer _ )   -> pure ()
    IT (_ := U Id _) -> do
      RegDesc{values, ss} <- liftIO $ readArray rd (num x)
      liftIO $ writeArray rd (num x) (RegDesc (op:values) (ss'+ss) True)
    _ -> liftIO $ writeArray rd (num x) (RegDesc [op] ss' True)

  pure x

load :: Register -> Operand -> MIPSMonad ()
load r op = do
  h <- use home
  case op `Map.lookup` h of
    Nothing -> tell1 $ case op of
      RF name -> LoadFG r name
      R name  -> LoadWG r name
      _ -> internal $
        "Reading from Constant or unavailable Temp " <> emit op
    Just offset -> tell1 $ if isFloat op 
      then LoadF r (offset, FP)
      else LoadW r (offset, FP)



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

  TT (Br _)             -> False
  TT (IfBr x _ _)       -> x == op
  TT (CondBr _ x y _ _) -> x == op || y == op
  TT (Return)           -> False
  TT (Exit)             -> False

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

isFloat = \case
    RF _ -> True
    TF _ -> True
    _    -> False


-- freshRegs :: Int -> MIPSMonad [ Word32 ]
-- freshRegs 1 = (:[]) <$> freshReg
-- freshRegs n = do
--   rs <- freshRegs (n-1)
--   r  <- freshReg' rs
--   pure $ r : rs

freshReg :: Operand -> MIPSMonad Register
freshReg = freshReg' []

freshReg' :: [ Word32 ] -> Operand -> MIPSMonad Register
freshReg' exceptions op = do
  rd <- if isFloat op
    then use floatregs
    else use registers 

  h  <- use home
  n  <- fromMaybe
    (internal $ "couldn't find a fresh reg meeting exceptions " <> show exceptions) <$>
    (min' rd $ if isFloat op then 30 else 21)
  d@RegDesc {values, dirty} <- liftIO $ readArray rd n
  
  when dirty $ do
    liftIO $ writeArray rd n d { dirty = False }
    forM_ values $ \v -> do
      case v `Map.lookup` h of
        Nothing     -> case v of
          (R  name) -> tell1 $ StoreWG (General n) name
          (RF name) -> tell1 $ StoreFG (FloatP n) name

          temp@(T _)  -> do
            tell [ BinOpi AddI SP SP (IC $ -4)
                 , StoreW (General n) (0, SP) ]
            toffs <- vsp <-= 4
            home %= Map.insert temp toffs

          temp@(TF _)  -> do
            tell [ BinOpi AddI SP SP (IC $ -4)
                 , StoreF (FloatP n) (0, SP) ]
            toffs <- vsp <-= 4
            home %= Map.insert temp toffs

          _        -> internal "trying to save a constant operand as global 2"
        Just offset ->
          tell1 $ if isFloat op
            then StoreF (FloatP  n) (offset, FP)
            else StoreW (General n) (offset, FP)
      variables %= Map.delete v
  pure $ if isFloat op
    then FloatP n
    else General n 

  where
    min' :: IOArray Word32 RegDesc -> Word32 -> MIPSMonad (Maybe Word32)
    min' regs n = min'' 0 Nothing


      where
        min'' :: Word32 -> (Maybe (Word32, Word32)) -> MIPSMonad (Maybe Word32)
        min'' i mb | i == n = case mb of 
          (Just (b, bc)) -> pure . Just $ b
          Nothing        -> pure Nothing
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
      tell 
        [ DataSection
        , Align
        , Data "_base_header" 8
        , Align
        , Data "_last_used" 4
        , MString "_true" "true\n"
        , MString "_false" "false\n" ]

      mapM_ gmips datas

    unless (null modules) $ do
      tell1 TextSection
      tell1 $ Global "main"
      mapM_ gmips modules

instance Gmips Data where
  gmips VarData { dName, dSpace } = do
    tell 
      [ Align
      , Data dName dSpace ]
  gmips StringData { dName, dString } = do
    tell1 $ MString dName dString

instance Gmips Module where
  gmips Module { mName, mBlocks } = do
    tell1 $ Comment mName
    mapM_ gmips mBlocks
    resetRegDescs
    variables .= Map.empty
    home      .= Map.empty

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
      let 
        (f, load') = if isFloat op0 
          then (ScratchF, LoadFI) 
          else (Scratch , LoadI)
      (x, y) <- case (op0, op1) of
        (C c1, C c2) -> do 
          tell 
            [ load' (f 1) c1
            , load' (f 1) c1]
          pure (f 0, f 1)
        
        (   _, C c2) -> do 
          tell1 $ load' (f 0) c2
          x <- getReg1 term 
          pure (x, f 0)
        
        (C c1, _   ) -> do 
          tell1 $ load' (f 0) c1
          y <- getReg1 term 
          pure (f 0, y)
        
        _            -> getReg2 term


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
  gmips tac = (tell1 $ Comment $ "  " <> emit tac) >> case tac of
    TAC.Comment str -> tell1 $ Comment str

    Var _ name offs _ t -> case t of 
      Basic {atom = EpFloat} -> home %= Map.insert (RF  name) offs
      _                      -> home %= Map.insert (R name) offs

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
        | floatconst r -> do
          (x, y) <- getReg2 tac
          tell
            [ LoadFI (ScratchF 0) (extract r)
            , BinOp o x (ScratchF 0) y ]
        | floatconst l -> do
          (x, y) <- getReg2 tac
          tell
            [ LoadFI (ScratchF 0) (extract l)
            , BinOp o x y (ScratchF 0) ]
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
            [ LoadFI (ScratchF 0) (IC 0)
            , BinOpi SubF x (ScratchF 0) c ]
          NegI -> tell1 $ BinOpi SubI x Zero c
          BNot -> tell1 $ BinOpi BXor x Zero c
          Id   -> do 
            tell1 $ if isFloat op 
              then LoadFI x c
              else LoadI  x c
      U Id u -> do
        x <- getReg1 tac      
        variables %= Map.insert op x

      U o u -> do
        (x, y) <- getReg2 tac
        case o of
          NegF -> tell
            [ LoadFI (ScratchF 0) (IC 0)
            , BinOp SubF x (ScratchF 0) y ]
          NegI -> tell1 $ BinOp SubI x Zero y
          BNot -> tell1 $ BinOp BXor x Zero y

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

    a :=@ (b@(T num), c) -> gmips (a := B AddI b c)

    _ :=@ (C _, _) -> internal "_ :=@ (Constant, _)"

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
      x <- getReg1 tac
      tell1 $ BinOpi AddI SP SP (IC $ -4)
      tell1 $ if isFloat op 
        then StoreF x (0, SP) 
        else StoreW x (0, SP)

    RefParam o@(R name) -> do
      h <- use home
      tell
        [ BinOpi AddI SP SP (IC $ -4)
        , case o `Map.lookup` h of
            Nothing     -> LoadA (Scratch 0) name
            Just offset -> BinOpi AddI (Scratch 0) FP (IC offset)
        , StoreW (Scratch 0) (0, SP) ]

    RefParam _ -> internal "RefParam (Const/Temp)"

    Call proc -> do 
      rdg <- use registers
      rdf <- use floatregs
      cleanReg [0..20] rdg False
      cleanReg [0..29] rdf True
      variables .= Map.empty
        
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
      where 
        cleanReg :: [ Word32 ] -> IOArray Word32 RegDesc -> Bool -> MIPSMonad ()
        cleanReg range rd isF = do
          h <- use home
          forM_ range $ \i -> do
            RegDesc {dirty, values} <- liftIO $ readArray rd i
            when dirty . forM_ values $ \v -> do
              case v `Map.lookup` h of
                Nothing     -> case v of
                  (R  name) -> tell1 $ StoreWG (General i) name
                  (RF name) -> tell1 $ StoreFG (FloatP i) name

                  temp@(T _)  -> do
                    tell [ BinOpi AddI SP SP (IC $ -4)
                         , StoreW (General i) (0, SP) ]
                    toffs <- vsp <-= 4
                    home %= Map.insert temp toffs

                  temp@(TF _)  -> do
                    tell [ BinOpi AddI SP SP (IC $ -4)
                         , StoreF (FloatP i) (0, SP) ]
                    toffs <- vsp <-= 4
                    home %= Map.insert temp toffs

                  _        -> internal $ "trying to save a constant operand as global " <> emit v
                Just offset ->
                  tell1 $ if isF
                    then StoreF (FloatP  i) (offset, FP)
                    else StoreW (General i) (offset, FP)
            liftIO $ writeArray rd i (RegDesc [] 0 False)

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
