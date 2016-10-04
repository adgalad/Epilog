{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Epilog.IR.Expression
  ( irExpression
  , irBoolean
  , irLval
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression hiding (Not)
import qualified Language.Epilog.AST.Expression as E (UnaryOp (Not))
import           Language.Epilog.Common
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Debug.Trace
--------------------------------------------------------------------------------

irExpression :: Expression -> IRMonad Operand
irExpression e@Expression { exp' } = case exp' of
  LitBool  b -> pure . C . BC $ b
  LitChar  c -> pure . C . CC $ c
  LitInt   i -> pure . C . IC $ i
  LitFloat f -> pure . C . FC $ f

  LitString _str -> internal "String literals are not implemented yet."

  Rval lval -> do
    r <- irLval lval

    t <- newTemp
    addTAC $ t :=* r
    pure t
--
  ECall _procName _args -> internal "Procedure calls are not implemented yet."

  Binary op exp0 exp1 -> if
    | op `elem` [Andalso, Orelse] -> wrapBoolean e

    | otherwise -> do
      operand0 <- irExpression exp0
      operand1 <- irExpression exp1
      result   <- newTemp

      let Basic { atom } = expType exp0

      addTAC $ result := B (toIRBOp atom op) operand0 operand1
      pure result

  Unary op exp0 -> if
    | op == E.Not -> wrapBoolean e -- do

    | otherwise -> do
      operand0 <- irExpression exp0
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
irLval Lval { lval' } = case lval' of
  Variable name -> pure $ R name
  -- Member   lval name ->
  --   pure ()
  -- Index    lval idx ->
  --   pure ()
  Deref lval -> do
    r <- irLval lval
    t <- newTemp
    addTAC $ t :=* r
    pure t
