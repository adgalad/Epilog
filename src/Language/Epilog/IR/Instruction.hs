{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Epilog.IR.Instruction
  ( irInstruction
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression  hiding (VarKind (..))
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Common
import           Language.Epilog.IR.Expression
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Position
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (use, (%=))
import           Control.Monad                   (void)
--------------------------------------------------------------------------------

irInstruction :: Instruction -> IRMonad ()
irInstruction = \case
  Assign { instP, assignTarget, assignVal } -> do
    comment $ "Assignment at " <> showP instP
    t <- irExpression assignVal
    r <- irLval assignTarget
    addTAC $ r :*= t

  ICall { instP {-, callName, callArgs-} } ->
    comment $ "Call args at " <> showP instP
    -- mapM_ irExpression callArgs

  If { instP, ifGuards } -> do
    comment $ "If at " <> showP instP

    next <- newLabel
    nextBlock <|= next

    irGuards next . toList $ ifGuards

    nextBlock %= tail

    (next #)

  For { instP , forVar, forRanges } -> do
    comment $ "For at " <> showP instP

    irRange forVar $ toList forRanges

  While { instP, whileGuards } -> do
    comment $ "While at " <> showP instP

    whileHeader <- newLabel
    terminate $ Br whileHeader
    nextBlock <|= whileHeader

    (whileHeader #)
    next <- newLabel
    irGuards next . toList $ whileGuards

    (next #)

  Read { instP, readTarget } -> do
    comment $ "Read at " <> showP instP

    let readFunc = case lvalType readTarget of
          Basic { atom } -> case atom of
            EpBoolean   -> "readBoolean"
            EpFloat     -> "readFloat"
            EpInteger   -> "readInteger"
            EpCharacter -> "readChar"
          _ -> internal "non-printable type"

    t <- newTemp
    addTAC $ t :<- readFunc

    r <- irLval readTarget
    addTAC $ r :*= t

  Write { instP, writeVal } -> do
    comment $ "Write at " <> showP instP

    let writeFunc = case expType writeVal of
          Basic { atom } -> case atom of
            EpBoolean   -> "writeBoolean"
            EpFloat     -> "writeFloat"
            EpInteger   -> "writeInteger"
            EpCharacter -> "writeChar"
          _ -> internal "non-printable type"

    t <- irExpression writeVal

    addTAC $ Param t
    addTAC $ Call writeFunc

  Answer { instP, answerVal } -> do
    comment $ "Answer at " <> showP instP
    void $ irExpression answerVal

  Finish { instP } ->
    comment $ "Finish at " <> showP instP

irGuards :: Label -> [(Position, Expression, Insts)] -> IRMonad ()
irGuards _ [] = internal "impossible call to irGuards"
irGuards final ((guardP, cond, insts):gs) = do
  comment $ "Guard at " <> showP guardP

  true  <- newLabel
  false <- case gs of
    [] -> pure final
    _  -> newLabel
  irBoolean true false cond

  (true #)
  mapM_ irInstruction insts

  next : _ <- use nextBlock
  use currentBlock >>= \case
    Nothing -> pure ()
    Just _  -> terminate $ Br next

  case gs of
    [] -> pure ()
    _  -> do
      (false #)
      irGuards final gs

irRange :: Name -> [Range] -> IRMonad ()
irRange _  [] = pure ()
irRange var ((_, low, high, insts):rs) = do
  lOp <- irExpression low
  hOp <- irExpression high

  addTAC $ R var := Id lOp

  gHeader <- newLabel
  gBody   <- newLabel
  next    <- newLabel
  terminate $ Br gHeader

  (gHeader #)
  terminate $ CondBr LEF (R var) hOp gBody next

  (gBody #)
  mapM_ irInstruction insts
  let 
    plusOne = case expType low of 
      Basic { atom } | atom == EpCharacter -> C $ CC 1
                     | atom == EpInteger   -> C $ IC 1
      _ -> internal "bad type in for bounds"

  addTAC $ R var := B AddI (R var) plusOne


  terminate $ Br gHeader

  (next #)
  irRange var rs

  





