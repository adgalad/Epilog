{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Epilog.IR.Instruction
  ( irInstruction
  , irIBlock
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression  hiding (VarKind (..))
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Common
import           Language.Epilog.IR.Expression
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC          hiding (TAC (Answer, Var))
import qualified Language.Epilog.IR.TAC          as TAC (TAC (Answer, Var))
import           Language.Epilog.Position
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (use, (%=))
import qualified Data.Sequence                   as Seq (reverse)
--------------------------------------------------------------------------------

irInstruction :: Instruction -> IRMonad ()
irInstruction = \case
  Assign { instP, assignTarget, assignVal } -> do
    comment $ "Assignment at " <> showP instP
    t <- irExpression assignVal
    r <- irLval assignTarget
    addTAC $ r :*= t

  ICall { instP , callName, callArgs, callRetType } -> do
    comment $ "Call at " <> showP instP
    args <- mapM (either irLval irExpression) callArgs
    mapM_ (addTAC . Param) (Seq.reverse args)
    addTAC $ Call callName
    -- addTAC . Cleanup . (*4) . fromIntegral . length $ callArgs
    addTAC . Cleanup . fromIntegral . sizeT $ callRetType

  If { instP, ifGuards } -> do
    comment $ "If at " <> showP instP

    next <- newLabel
    nextBlock <|= next

    irGuards next . toList $ ifGuards

    nextBlock %= tail

    (next #)

  For { instP , forVar, forRanges } -> do
    comment $ "For at " <> showP instP
    iterator <- irLval forVar
    irRange iterator $ toList forRanges

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
            EpBoolean   -> "_readBoolean"
            EpFloat     -> "_readFloat"
            EpInteger   -> "_readInteger"
            EpCharacter -> "_readChar"
            _           -> internal "non-readable type"
          _ -> internal "non-readable type"

    t <- newTemp
    addTAC $ t :<- readFunc

    r <- irLval readTarget
    addTAC $ r :*= t

  Write { instP, writeVal } -> do
    comment $ "Write at " <> showP instP

    let writeFunc = case expType writeVal of
          Basic { atom } -> case atom of
            EpBoolean   -> "_writeBoolean"
            EpFloat     -> "_writeFloat"
            EpInteger   -> "_writeInteger"
            EpCharacter -> "_writeChar"
            _           -> internal "non-printable type"
          EpStr _ _     -> "_writeStr"
          _ -> internal "non-printable type"

    t <- irExpression writeVal

    addTAC $ Param t
    addTAC $ Call writeFunc

  Make { instP, makeTarget } -> do
    comment $ "Make at " <> showP instP

    t <- newTemp
    addTAC . Param . C . IC . fromIntegral . sizeT . lvalType $ makeTarget
    addTAC $ t :<- "_make"
    addTAC $ Cleanup 4

    r <- irLval makeTarget
    addTAC $ r :*= t

  Ekam { instP, ekamTarget } -> do
    comment $ "Ekam at " <> showP instP

    r <- irLval ekamTarget
    addTAC $ Param r
    addTAC $ Call "_ekam"
    addTAC $ Cleanup 4

    addTAC $ r :*= C (IC 0)

  Answer { instP, answerVal } -> do
    comment $ "Answer at " <> showP instP
    t0 <- irExpression answerVal

    addTAC $ TAC.Answer t0

    use retLabel >>= \case
      Nothing -> internal "nowhere to return after answer"
      Just l  -> terminate $ Br l

  Finish { instP } -> do
    comment $ "Finish at " <> showP instP
    use retLabel >>= \case
      Nothing -> internal "nowhere to return after finish"
      Just l  -> terminate $ Br l

  Var { varName, varOffset, varSize } ->
    addTAC $ TAC.Var False varName (negate $ 4 + varOffset) varSize

irGuards :: Label -> [(Position, Expression, IBlock)] -> IRMonad ()
irGuards _ [] = internal "impossible call to irGuards"
irGuards final ((guardP, cond, iblock):gs) = do
  comment $ "Guard at " <> showP guardP

  true  <- newLabel
  false <- case gs of
    [] -> pure final
    _  -> newLabel
  irBoolean true false cond

  (true #)
  irIBlock iblock

  next : _ <- use nextBlock
  use currentBlock >>= \case
    Nothing -> pure ()
    Just _  -> terminate $ Br next


  case gs of
    [] -> pure ()
    _  -> do
      (false #)
      irGuards final gs

irRange :: Operand -> [Range] -> IRMonad ()
irRange _ [] = pure ()
irRange iterator ((rangeP, low, high, iblock) : rs) = do
  comment $ "Range at " <> showP rangeP

  lOp <- irExpression low
  hOp <- irExpression high

  addTAC $ iterator :*= lOp

  gHeader <- newLabel
  gBody   <- newLabel
  next    <- newLabel
  terminate $ Br gHeader

  (gHeader #)
  t0 <- newTemp
  addTAC $ t0 :=* iterator
  terminate $ CondBr LEI t0 hOp gBody next

  (gBody #)
  irIBlock iblock

  let
    one = case expType low of
      Basic { atom } | atom == EpCharacter -> C $ CC 1
                     | atom == EpInteger   -> C $ IC 1
      _ -> internal "bad type in for bounds"

  t1 <- newTemp
  t2 <- newTemp
  addTAC $ t1 :=* iterator
  addTAC $ t2 := B AddI t1 one
  addTAC $ iterator :*= t2

  terminate $ Br gHeader

  (next #)
  irRange iterator rs

irIBlock :: IBlock -> IRMonad ()
irIBlock (IBlock insts) = do
  enterScope
  mapM_ irInstruction insts
  exitScope
