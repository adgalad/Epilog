{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE EmptyCase        #-}

module Language.Epilog.IR.Expression
  ( irExpression
  , irBoolean
  , irLval
  , irLvalAddr
  , Metaoperand (..)
  , Meta (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression hiding (Not, VarKind (..))
import qualified Language.Epilog.AST.Expression as E (UnaryOp (Not))
import qualified Language.Epilog.AST.Expression as K (VarKind (..))
import           Language.Epilog.Common
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Data.Bits                      ((.&.), (.|.), shiftR, shiftL,
                                                 xor, complement)
import qualified Data.Sequence                  as Seq (reverse)
--------------------------------------------------------------------------------

irExpression :: Expression -> IRMonad Operand
irExpression e@Expression { exp', expPos, expType = ty } = case exp' of
  LitBool  b -> pure . C . BC $ b
  LitChar  c -> pure . C . CC $ c
  LitInt   i -> pure . C . IC $ i
  LitFloat f -> pure . C . FC $ f

  LitString idx -> pure $ R ("_str" <> show idx)

  Void -> pure . C . IC $ 0

  Rval rval -> do
    r <- irLval rval
    case r of
      Single (Pure op) -> pure $ op

      Single (Star op) -> do
        t <- newTemp ty
        addTAC $ t :=* op
        pure t

      Brackets (Pure b) off -> do
        t <- newTemp ty
        addTAC $ t :=# (b, off)
        pure t

      Brackets (Star b) off -> do
        t1 <- newTempG
        t2 <- newTemp ty
        addTAC $ t1 := B AddI b off
        addTAC $ t2 :=* t1
        pure t2

  ECall callName callArgs -> do
    args <- mapM (either irLvalAddr irExpression) callArgs
    mapM_ (addTAC . Param) (Seq.reverse args)

    t <- newTemp ty
    addTAC $ t :<- callName
    addTAC $ Cleanup (4 * fromIntegral (length callArgs))

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

        (C (IC i0), C (IC i1)) -> if op `elem` [IntDiv, Rem] && i1 == 0
          then internal $ "Division by zero " <> show expPos
          else pure . C . IC $ i0 `op'` i1

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
          result   <- newTemp (expType exp0)

          let Basic { atom } = expType exp0

          -- addTAC $ result := B (toIRBOp atom op) operand0 operand1
          case operand0 of
            C _ -> do
              t <- newTemp (expType exp0)
              addTAC $ t := U Id operand0
              addTAC $ result := B (toIRBOp atom op) t operand1
            _ ->
              addTAC $ result := B (toIRBOp atom op) operand0 operand1
          pure result

  Unary op exp0 -> if
    | op == E.Not -> wrapBoolean e

    | otherwise -> do
      operand0 <- irExpression exp0

      case operand0 of
        C (BC b0) -> pure . C $ op' b0
          where
            op' = case op of
              E.Not -> BC . not
              ToF   -> FC . (\x -> if x then 1.0 else 0.0)
              ToI   -> IC . (\x -> if x then 1   else 0  )
              ToC   -> CC . (\x -> if x then 1   else 0)

              _ -> internal "badOp"

        C (IC i0) -> pure . C $ op' i0
          where
            op' = case op of
              Uminus -> IC . negate
              Bnot   -> IC . complement
              ToF    -> FC . fromIntegral
              ToC    -> CC . fromIntegral
              ToB    -> BC . (/= 0)
              _ -> internal "badOp"

        C (FC f0) -> pure . C $ op' f0
          where
            op' = case op of
              Uminus -> FC . negate
              ToC    -> CC . truncate
              ToI    -> IC . truncate
              ToB    -> BC . (/= 0.0)
              _ -> internal "badOp"

        C (CC c0) -> pure . C $ op' c0
          where
            op' = case op of
              ToF -> FC . fromIntegral 
              ToI -> IC . fromIntegral 
              ToB -> BC . (/= 0)
              _ -> internal "badOp"

        _ -> do
          result   <- newTemp ty

          let Basic { atom } = expType exp0

          addTAC $ result := U (toIRUOp atom op) operand0
          pure result

wrapBoolean :: Expression -> IRMonad Operand
wrapBoolean e = do
  true <- newLabel "True"
  false <- newLabel "False"

  result <- newTemp (expType e)
  irBoolean true false e

  finish <- newLabel "WrapBoolFinish"

  (true #)
  addTAC $ result := U Id (C . BC $ True)
  terminate $ Br finish

  (false #)
  addTAC $ result := U Id (C . BC $ False)
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
  ToB    -> case atom of
    EpFloat -> FtoI
    _       -> Id
  ToI    -> case atom of
    EpFloat -> FtoI
    _       -> Id
  ToC    -> case atom of
    EpFloat -> FtoI
    _       -> Id
  ToF    -> case atom of
    EpFloat -> Id
    _       -> ItoF
  Uminus -> case atom of
    EpFloat   -> NegF
    EpInteger -> NegI
    _         -> internal $ "Operator Uminus received wrong type `" <> show atom <> "`"


irBoolean :: Label -> Label -> Expression -> IRMonad ()
irBoolean true false e@Expression { exp', expPos } = case exp' of
  LitBool   b -> terminate . Br $ if b then true else false

  LitChar   _ -> internal "litChar cannot be a boolean expression"
  LitInt    _ -> internal "litInt cannot be a boolean expression"
  LitFloat  _ -> internal "litFloat cannot be a boolean expression"
  LitString _ -> internal "litString cannot be a boolean expression"
  Void        -> internal "void cannot be a boolean expression"

  Rval _lval -> do
    t <- irExpression e
    terminate $ IfBr t true false

  ECall callName callArgs -> do
    args <- mapM (either irLvalAddr irExpression) callArgs
    mapM_ (addTAC . Param) (Seq.reverse args)

    t <- newTempG
    addTAC $ t :<- callName
    addTAC $ Cleanup (4 * fromIntegral (length callArgs))

    terminate $ IfBr t true false

  Binary op exp0 exp1 -> case op of
    And -> do
      t <- irExpression e
      terminate $ IfBr t true false

    Andalso -> do
      middle <- newLabel "JC_andalso"
      irBoolean middle false exp0

      (middle #)
      irBoolean true false exp1

    Or -> do
      t <- irExpression e
      terminate $ IfBr t true false

    Orelse  -> do
      middle <- newLabel "JC_orelse"
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

        (C (IC i0), C (IC i1)) -> if op `elem` [FAop, NFop] && i1 == 0
          then internal $ "Division by zero " <> show expPos
          else terminate . Br $ if i0 `cond` i1 then true else false
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

          terminate CondBr
            { rel       = toIRRel (expType exp0) op
            , op0
            , op1
            , trueDest  = true
            , falseDest = false }

  Unary op exp0 -> case op of
    E.Not -> irBoolean false true exp0
    _     -> internal $ "Non-boolean operator `" <> show op <> "`"


toIRRel :: Type -> BinaryOp -> Rel
toIRRel t = \case
  LTop     -> if
    | t == floatT -> LTF
    | t `elem` [intT, charT] -> LTI
    | otherwise -> internal "wut?"

  LEop     -> if
    | t == floatT -> LEF
    | t `elem` [intT, charT] -> LEI
    | otherwise -> internal "wut?"

  GTop     -> if
    | t == floatT -> GTF
    | t `elem` [intT, charT] -> GTI
    | otherwise -> internal "wut?"

  GEop     -> if
    | t == floatT -> GEF
    | t `elem` [intT, charT] -> GEI
    | otherwise -> internal "wut?"

  EQop     -> if
    | t == floatT -> EQF
    | t `elem` [intT, charT, ptrT] -> EQI
    | otherwise -> internal "wut?"

  NEop     -> if
    | t == floatT -> NEF
    | t `elem` [intT, charT, ptrT] -> NEI
    | otherwise -> internal "wut?"

  FAop     -> FAI
  NFop     -> NFI
  o        -> internal $ "Non relational operator `" <> show o <> "`"


data Meta
  = Pure { op :: Operand }
  | Star { op :: Operand }
  deriving (Eq, Show, Ord)

data Metaoperand
  = Single   { base :: Meta }
  | Brackets { base :: Meta, off :: Operand } -- ^ the offset is in bytes
  deriving (Eq, Show, Ord)

irLval :: Lval -> IRMonad Metaoperand
irLval Lval { lvalType, lval' } = case lval' of
  Variable name k _offset -> do
    comment $ "Lval " <> show k <> " variable `" <> name <> "`"
    name' <- getVarName name

    pure $ case k of
      K.RefParam -> Single . Star . R $ name'
      _          -> 
        let r = case lvalType of
              Basic {atom = EpFloat} -> RF
              _ -> R
        in Single . Pure . r $ name'

  Member lval _name offset -> do
    irLval lval >>= \case
      Single b -> pure $ Brackets b (C . IC . fromIntegral $ offset)

      Brackets b off -> case off of
        C (IC n) -> pure $ 
          Brackets b (C . IC . (+n) . fromIntegral $ offset)
        _ -> do
          t <- newTempG
          addTAC $ t := B AddI off (C . IC . fromIntegral $ offset)
          pure $ Brackets b t

  Index lval idx -> do
    r  <- irLval lval
    t0 <- irExpression idx
    case r of
      Single b -> case t0 of
        C (IC n) -> pure $ Brackets b (C . IC . (*n) . fromIntegral . sizeT $ lvalType)
        _ -> do
          t1 <- newTempG
          addTAC $ t1 := B MulI t0 (C . IC . fromIntegral . sizeT $ lvalType)
          pure $ Brackets b t1

      Brackets b off -> case (off, t0) of
        (C (IC n), C (IC m)) -> pure $
          Brackets b (C (IC $ n + (m * (fromIntegral . sizeT $ lvalType))))
        (_, C (IC m)) -> do
          t1 <- newTempG
          addTAC $ t1 := B AddI off (C . IC . (*m) . fromIntegral . sizeT $ lvalType)
          pure $ Brackets b t1
        _ -> do
          t1 <- newTempG
          addTAC $ t1 := B MulI t0 (C . IC . fromIntegral . sizeT $ lvalType)
          t2 <- newTempG
          addTAC $ t2 := B AddI off t1
          pure $ Brackets b t2

  Deref lval -> do
    irLval lval >>= \case
      Single (Pure op) -> 
        pure $ Single (Star op)

      Single (Star op) -> do
        t <- newTemp lvalType
        addTAC $ t :=* op
        pure $ Single (Star t)

      Brackets (Pure op) off -> do
        t <- newTemp lvalType
        addTAC $ t :=# (op, off)
        pure $ Single (Star t)

      Brackets (Star op) off -> do
        t <- newTemp lvalType
        addTAC $ t := B AddI op off
        pure $ Single (Star t)

irLvalAddr :: Lval -> IRMonad Operand
irLvalAddr lval = do
  irLval lval >>= \case
    Single (Pure op) -> do
      t <- newTempG
      addTAC $ t :=& op
      pure t

    Single (Star op) -> pure op

    Brackets (Pure b) off -> do
      t <- newTempG
      addTAC $ t :=@ (b, off)
      pure t

    Brackets (Star b) off -> do
      t <- newTempG
      addTAC $ t := B AddI b off
      pure t
