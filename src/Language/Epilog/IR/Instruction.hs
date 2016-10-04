{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Epilog.IR.Instruction
  ( irInstruction
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Common
import           Language.Epilog.IR.Expression
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Position
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Monad                   (void)
--------------------------------------------------------------------------------

irInstruction :: Instruction -> IRMonad ()
irInstruction = \case
  Assign { instP, assignTarget, assignVal } -> do
    addTAC . Comment $ "Assignment at " <> showP instP
    t <- irExpression assignVal
    r <- irLval assignTarget
    addTAC $ r :*= t

  ICall { instP {-, callName-}, callArgs } -> do
    addTAC . Comment $ "Call args at " <> showP instP
    mapM_ irExpression callArgs

  If { instP, ifGuards } -> do
    ifNext <- newLabel
    addTAC . Comment $ "If at " <> showP instP

    mapM_ (irGuard ifNext) ifGuards

    terminate $ Br ifNext

    (ifNext #)

  For { instP {-, forVar, forRanges-} } -> -- TODO
    addTAC . Comment $ "For at " <> showP instP

  While { instP, whileGuards } -> do
    whileNext <- newLabel
    addTAC . Comment $ "While at " <> showP instP

    terminate $ Br whileNext

    (whileNext #)
    mapM_ (irGuard whileNext) whileGuards

  Read { instP, readTarget } -> do
    addTAC . Comment $ "Read at " <> showP instP

    let readFunc = case lvalType readTarget of
          Basic { atom } -> case atom of
            EpBoolean   -> "readBoolean"
            EpFloat     -> "readFloat"
            EpInteger   -> "readInteger"
            EpCharacter -> "readChar"
          _ -> internal "non-printable type"

    t <- newTemp
    after <- newLabel
    terminate $ t :<- (readFunc, after)

    (after #)
    r <- irLval readTarget
    addTAC $ r :*= t

  Write { instP, writeVal } -> do
    addTAC . Comment $ "Write at " <> showP instP

    let writeFunc = case expType writeVal of
          Basic { atom } -> case atom of
            EpBoolean   -> "writeBoolean"
            EpFloat     -> "writeFloat"
            EpInteger   -> "writeInteger"
            EpCharacter -> "writeChar"
          _ -> internal "non-printable type"

    t <- irExpression writeVal

    addTAC $ Param t

    after <- newLabel
    terminate $ CallThen writeFunc after

    (after #)

  Answer { instP, answerVal } -> do
    addTAC . Comment $ "Answer at " <> showP instP
    void $ irExpression answerVal

  Finish { instP } ->
    addTAC . Comment $ "Finish at " <> showP instP

irGuard :: Label -> (Position, Expression, Insts) -> IRMonad ()
irGuard lbl (guardP, cond, insts) = do
  addTAC . Comment $ "Guard at " <> showP guardP

  true  <- newLabel
  false <- newLabel
  irBoolean true false cond

  (true #)
  mapM_ irInstruction insts
  terminate $ Br lbl

  (false #)