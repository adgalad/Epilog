{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE EmptyCase #-}

module Language.Epilog.IR.Expression
  ( irExpression
  , irBoolean
  , irLval
  , irLvalAddr
  , Metaoperand (..)
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
import           Data.Bits                      ((.&.), (.|.), shiftR, shiftL,
                                                 xor, complement)
import qualified Data.Sequence                  as Seq (reverse)
--------------------------------------------------------------------------------

irExpression :: Expression -> IRMonad Operand
irExpression e@Expression { exp', expPos, expType = t } = case exp' of
  LitBool  b -> pure . C . BC $ b
  LitChar  c -> pure . C . CC $ c
  LitInt   i -> pure . C . IC $ i
  LitFloat f -> pure . C . FC $ f

  LitString idx -> pure $ R ("_str" <> show idx)

  Void -> pure . C . IC $ 0

  Rval rval -> do
    r <- irLval rval
    case r of
      Pure op -> pure $ op

      Brackets b off -> do
        t <- newTemp t
        addTAC $ t :=# (b, off)
        pure t

      Star op -> do
        t <- newTemp t
        addTAC $ t :=* op
        pure t

  ECall callName callArgs -> do
    args <- mapM (either irLvalAddr irExpression) callArgs
    mapM_ (addTAC . Param) (Seq.reverse args)

    t <- newTemp t
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
          result   <- newTemp (expType exp0)

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

  Rval _lval -> do
    t <- irExpression e
    terminate $ IfBr t true false

  ECall _procName _args -> internal "Procedure calls are not implemented yet."

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

data Metaoperand
  = Pure     { op :: Operand}
  | Brackets { base :: Operand, off :: Operand } -- ^ the offset is in bytes
  | Star     { op :: Operand}
  deriving (Eq, Show, Ord)

regKind :: Type -> (String -> Operand)
regKind = \case
  Basic {atom = EpFloat} -> RF
  _                      -> R

irLval :: Lval -> IRMonad Metaoperand
irLval Lval { lvalType, lval' } = case lval' of
  Variable name k _offset -> do
    comment $ "Lval " <> show k <> " variable `" <> name <> "`"
    name' <- getVarName name
    pure . ($ (regKind lvalType) name') $ case k of
      K.RefParam -> Star
      _          -> Pure

  Member lval _name 0 -> irLval lval

  Member lval _name offset -> do
    r <- irLval lval
    case r of
      Pure op -> pure $ Brackets op (C . IC . fromIntegral $ offset)

      Brackets b off -> case off of
        C (IC n) -> case n of
          0 -> pure $ Brackets b (C . IC . fromIntegral $ offset)
          _ -> pure $ Brackets b (C . IC . (+n) . fromIntegral $ offset)
        _ -> do
          t <- newTempG
          addTAC $ t := B AddI off (C . IC . fromIntegral $ offset)
          pure $ Brackets b t

      Star op -> do
        t <- newTempG
        addTAC $ t :=* op
        pure $ Brackets t (C . IC . fromIntegral $ offset)

  Index lval idx -> do
    r  <- irLval lval
    t0 <- irExpression idx
    case r of
      Pure op -> case t0 of
        C (IC n) -> pure $ case n of
          0 -> Star op
          _ -> Brackets op (C . IC . (*n) . fromIntegral . sizeT $ lvalType)
        _ -> do
          t1 <- newTempG
          addTAC $ t1 := B MulI t0 (C . IC . fromIntegral . sizeT $ lvalType)
          pure $ Brackets op t1

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

      Star op -> do
        t1 <- newTempG
        addTAC $ t1 :=* op
        case t0 of
          C (IC n) -> pure $ case n of
            0 -> Star t1
            _ -> Brackets t1 (C . IC . (*n) . fromIntegral . sizeT $ lvalType)
          _ -> do
            t2 <- newTempG
            addTAC $ t2 := B MulI t0 (C . IC . fromIntegral . sizeT $ lvalType)
            pure $ Brackets t1 t2

  Deref lval -> do
    r <- irLval lval
    case r of
      Pure op -> pure $ Star op

      Brackets b off -> do
        t <- newTemp lvalType
        addTAC $ t :=# (b, off)
        pure $ Star t

      Star op -> do
        t <- newTemp lvalType
        addTAC $ t :=* op
        pure $ Star t

irLvalAddr :: Lval -> IRMonad Operand
irLvalAddr lval = do
  r <- irLval lval
  case r of
    Pure op -> do
      t <- newTempG
      addTAC $ t :=& op
      pure t
    Brackets b off -> do
      t <- newTempG
      addTAC $ t :=@ (b, off)
      pure t
    Star op -> pure op
