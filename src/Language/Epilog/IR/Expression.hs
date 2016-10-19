{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE EmptyCase #-}

module Language.Epilog.IR.Expression
  ( irExpression
  , irBoolean
  , irLval
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression hiding (Not, VarKind(..))
import qualified Language.Epilog.AST.Expression as E (UnaryOp (Not))
import qualified Language.Epilog.AST.Expression as K (VarKind (..))
import           Language.Epilog.Common
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import Data.Bits ((.&.), (.|.), shiftR, shiftL, xor, complement)
--------------------------------------------------------------------------------

irExpression :: Expression -> IRMonad Operand
irExpression e@Expression { exp' } = case exp' of
  LitBool  b -> pure . C . BC $ b
  LitChar  c -> pure . C . CC $ c
  LitInt   i -> pure . C . IC $ i
  LitFloat f -> pure . C . FC $ f

  LitString idx -> pure $ R ("_str" <> show idx)

  Rval rval -> irRval rval

  ECall callName callArgs -> do
    args <- mapM irExpression callArgs
    mapM_ (addTAC . Param) args

    t <- newTemp
    addTAC $ t :<- callName

    addTAC . Cleanup . (*4) . fromIntegral . length $ callArgs

    pure t

  Binary op exp0 exp1 -> if
    | op `elem` [Andalso, Orelse, LTop, LEop, GTop, GEop, EQop, NEop, FAop, NFop] ->
      wrapBoolean e

    | otherwise -> do
      operand0 <- irExpression exp0
      operand1 <- irExpression exp1

      case (operand0, operand1) of
        (C (BC b0), C (BC b1)) -> pure . C . BC $ b0 `op'` b1
          where
            op' = case op of
              And -> (&&)
              Or  -> (||)
              Xor -> (/=)
              _ -> internal "badOp"

        (C (IC i0), C (IC i1)) -> pure . C . IC $ i0 `op'` i1
          where
            op' = case op of
              Band   -> (.&.)
              Bor    -> (.|.)
              Bsl    -> (. fromIntegral) . shiftL
              Bsr    -> (. fromIntegral) . shiftR
              Bxor   -> xor
              Plus   -> (+)
              Minus  -> (-)
              Times  -> (*)
              IntDiv -> div
              Rem    -> rem
              _ -> internal "badOp"

        (C (FC f0), C (FC f1)) -> pure . C . FC $ f0 `op'` f1
          where
            op' = case op of
              Plus     -> (+)
              Minus    -> (-)
              Times    -> (*)
              FloatDiv -> (/)
              _ -> internal "badOp"

        (C (CC c0), C (CC c1)) -> pure . C . CC $ c0 `op'` c1
          where op' = case op of

        _ -> do
          result   <- newTemp

          let Basic { atom } = expType exp0

          addTAC $ result := B (toIRBOp atom op) operand0 operand1
          pure result

  Unary op exp0 -> if
    | op == E.Not -> wrapBoolean e 

    | otherwise -> do
      operand0 <- irExpression exp0

      case operand0 of
        C (BC b0) -> pure . C . BC $ op' b0
          where
            op' = case op of
              E.Not -> not
              _ -> internal "badOp"

        C (IC i0) -> pure . C . IC $ op' i0
          where
            op' = case op of
              Uminus -> negate
              Bnot -> complement
              _ -> internal "badOp"

        C (FC f0) -> pure . C . FC $ op' f0
          where
            op' = case op of
              Uminus -> negate
              _ -> internal "badOp"

        C (CC c0) -> pure . C . CC $ op' c0
          where
            op' = case op of

        _ -> do
          result   <- newTemp

          let Basic { atom } = expType exp0

          addTAC $ result := U (toIRUOp atom op) operand0
          pure result

wrapBoolean :: Expression -> IRMonad Operand
wrapBoolean e = do
  true <- newLabel
  false <- newLabel

  result <- newTemp
  irBoolean true false e

  finish <- newLabel

  (true #)
  addTAC $ result := Id (C . BC $ True)
  terminate $ Br finish

  (false #)
  addTAC $ result := Id (C . BC $ False)
  terminate $ Br finish

  (finish #)

  pure result

toIRBOp :: Atom -> BinaryOp -> BOp
toIRBOp atom = \case
  And      -> BAnd
  Andalso  -> internal "Operator Andalso must be generated with irBoolean"
  Or       -> BOr
  Orelse   -> internal "Operator Orelse must be generated with irBoolean"
  Xor      -> BXor
  Band     -> BAnd
  Bor      -> BOr
  Bsl      -> BSL
  Bsr      -> BSR
  Bxor     -> BXor

  Plus     -> case atom of
    EpInteger -> AddI
    EpFloat   -> AddF
    _         -> internal $ "Operator Plus received wrong type `" <> show atom <> "`"
  Minus    -> case atom of
    EpInteger -> SubI
    EpFloat   -> SubF
    _         -> internal $ "Operator Minus received wrong type `" <> show atom <> "`"
  Times    -> case atom of
    EpInteger -> MulI
    EpFloat   -> MulF
    _         -> internal $ "Operator Times received wrong type `" <> show atom <> "`"

  FloatDiv -> DivF
  IntDiv   -> DivI
  Rem      -> RemI
  o        -> internal $ "Operator " <> show o <> " is a Rel, shouldn't be generated here."


toIRUOp :: Atom -> UnaryOp -> UOp
toIRUOp atom = \case
  E.Not  -> internal "Operator Not must be generated with irBoolean"
  Bnot   -> BNot
  Uminus -> case atom of
    EpFloat   -> NegF
    EpInteger -> NegI
    _         -> internal $ "Operator Uminus received wrong type `" <> show atom <> "`"


irBoolean :: Label -> Label -> Expression -> IRMonad ()
irBoolean true false e@Expression { exp' } = case exp' of
  LitBool   b -> terminate . Br $ if b then true else false

  LitChar   _ -> internal "litChar cannot be a boolean expression"
  LitInt    _ -> internal "litInt cannot be a boolean expression"
  LitFloat  _ -> internal "litFloat cannot be a boolean expression"
  LitString _ -> internal "litString cannot be a boolean expression"

  Rval _lval -> do
    t <- irExpression e
    terminate $ IfBr t true false

  ECall _procName _args -> internal "Procedure calls are not implemented yet."

  Binary op exp0 exp1 -> case op of
    And -> do
      t <- irExpression e
      terminate $ IfBr t true false

    Andalso -> do
      middle <- newLabel
      irBoolean middle false exp0

      (middle #)
      irBoolean true false exp1

    Or -> do
      t <- irExpression e
      terminate $ IfBr t true false

    Orelse  -> do
      middle <- newLabel
      irBoolean true middle exp0
      (middle #)
      irBoolean true false exp1

    _ -> do
      op0 <- irExpression exp0
      op1 <- irExpression exp1

      case (op0, op1) of
        (C (BC b0), C (BC b1)) -> terminate . Br $ if b0 `cond` b1 then true else false
          where
            cond = case op of
              LTop -> (<)
              LEop -> (<=)
              GTop -> (>)
              GEop -> (>=)
              EQop -> (==)
              NEop -> (/=)
              _ -> internal "badOp"

        (C (IC i0), C (IC i1)) -> terminate . Br $ if i0 `cond` i1 then true else false
          where
            cond = case op of
              LTop -> (<)
              LEop -> (<=)
              GTop -> (>)
              GEop -> (>=)
              EQop -> (==)
              NEop -> (/=)
              FAop -> \x y -> x `mod` y == 0
              NFop -> \x y -> x `mod` y /= 0
              _ -> internal "badOp"

        (C (FC f0), C (FC f1)) -> terminate . Br $ if f0 `cond` f1 then true else false
          where
            cond = case op of
              LTop -> (<)
              LEop -> (<=)
              GTop -> (>)
              GEop -> (>=)
              EQop -> (==)
              NEop -> (/=)
              _ -> internal "badOp"

        (C (CC c0), C (CC c1)) -> terminate . Br $ if c0 `cond` c1 then true else false
          where
            cond = case op of
              LTop -> (<)
              LEop -> (<=)
              GTop -> (>)
              GEop -> (>=)
              EQop -> (==)
              NEop -> (/=)
              _ -> internal "badOp"

        _ -> do

          let Basic { atom } = expType exp0

          terminate CondBr
            { rel       = toIRRel atom op
            , op0
            , op1
            , trueDest  = true
            , falseDest = false }

  Unary op exp0 -> case op of
    E.Not -> irBoolean false true exp0
    _     -> internal $ "Non-boolean operator `" <> show op <> "`"


toIRRel :: Atom -> BinaryOp -> Rel
toIRRel atom = \case
  LTop     -> if
    | atom == EpFloat -> LTF
    | atom `elem` [EpInteger, EpCharacter] -> LTI
    | otherwise -> internal "wut?"

  LEop     -> if
    | atom == EpFloat -> LEF
    | atom `elem` [EpInteger, EpCharacter] -> LEI
    | otherwise -> internal "wut?"

  GTop     -> if
    | atom == EpFloat -> GTF
    | atom `elem` [EpInteger, EpCharacter] -> GTI
    | otherwise -> internal "wut?"

  GEop     -> if
    | atom == EpFloat -> GEF
    | atom `elem` [EpInteger, EpCharacter] -> GEI
    | otherwise -> internal "wut?"

  EQop     -> if
    | atom == EpFloat -> EQF
    | atom `elem` [EpInteger, EpCharacter] -> EQI
    | otherwise -> internal "wut?"

  NEop     -> if
    | atom == EpFloat -> NEF
    | atom `elem` [EpInteger, EpCharacter] -> NEI
    | otherwise -> internal "wut?"

  FAop     -> FAI
  NFop     -> NFI
  o        -> internal $ "Non relational operator `" <> show o <> "`"

irLval :: Lval -> IRMonad Operand
irLval Lval { lvalType, lval' } = case lval' of
  Variable name k offset -> do
    comment $ show k <> " variable `" <> name <> "`"
    case k of
      K.Global -> pure $ R name
        -- if offset == 0
        --   then pure GP
        --   else do
        --     t <- newTemp
        --     addTAC $ t := B AddI GP base
        --     pure t
      K.Local -> do
        if offset == 0
          then pure FP
          else do
            t <- newTemp
            addTAC $ t := B AddI FP base
            pure t
      K.Param -> do
        t <- newTemp
        addTAC $ t := B AddI FP negBase
        pure t
    where
      base = C . IC . fromIntegral $ offset
      negBase = C . IC . fromIntegral . negate $ offset + (sizeT lvalType)

  Member lval _name offset -> do
    r <- irLval lval
    if offset == 0
      then pure r
      else do
        t <- newTemp
        addTAC $ t := B AddI r (C . IC . fromIntegral $ offset)
        pure t

  Index lval idx -> do
    r <- irLval lval
    t <- irExpression idx
    case t of
      C (IC 0) -> pure r
      _ -> do
        let sz = C . IC . fromIntegral . sizeT $ lvalType
        t1 <- newTemp
        addTAC $ t1 := B MulI sz t
        t2 <- newTemp
        addTAC $ t2 := B AddI r t1
        pure t2

  Deref lval -> do
    r <- irLval lval
    t <- newTemp
    addTAC $ t :=* r
    pure t


irRval :: Lval -> IRMonad Operand
irRval Lval { lvalType, lval' } = case lval' of
  Variable name k offset -> do
    comment $ show k <> " variable `" <> name <> "`"
    case k of
      K.Global -> pure $ R name
        -- if offset == 0
        --   then do
        --     t <- newTemp
        --     addTAC $ t :=* GP
        --     pure t
        --   else do
        --     t <- newTemp
        --     addTAC $ t :=# (offset', GP)
        --     pure t
      K.Local -> do
        if offset == 0
          then do
            t <- newTemp
            addTAC $ t :=* FP
            pure t
          else do
            t <- newTemp
            addTAC $ t :=# (offset', FP)
            pure t
      K.Param -> do
        t <- newTemp
        addTAC $ t :=# (negOffset, FP)
        pure t
    where
      negOffset = negate $ offset' + (fromIntegral $ sizeT lvalType)
      offset' = fromIntegral offset

  Member lval _name offset -> do
    r <- irLval lval
    if offset == 0
      then do
        t <- newTemp
        addTAC $ t :=* r
        pure t
      else do
        t1 <- newTemp
        addTAC $ t1 := B AddI r (C . IC . fromIntegral $ offset)
        t2 <- newTemp
        addTAC $ t2 :=* t1
        pure t2

  Index lval idx -> do
    r <- irLval lval
    t <- irExpression idx
    case t of
      C (IC 0) -> do
        t1 <- newTemp
        addTAC $ t1 :=* r
        pure r
      _ -> do
        let sz = C . IC . fromIntegral . sizeT $ lvalType
        t1 <- newTemp
        addTAC $ t1 := B MulI sz t
        t2 <- newTemp
        addTAC $ t2 := B AddI r t1
        t3 <- newTemp
        addTAC $ t3 :=* t2
        pure t3

  Deref lval -> do
    r <- irLval lval
    t1 <- newTemp
    t2 <- newTemp
    addTAC $ t1 :=* r
    addTAC $ t2 :=* t1
    pure t2
