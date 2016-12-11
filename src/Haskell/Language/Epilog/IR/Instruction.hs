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
import qualified Language.Epilog.IR.TAC          as TAC (TAC (Answer, Var, FloatVar))
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

    case r of
      Single (Pure op) ->
        addTAC $ op := U Id t

      Single (Star op) ->
        addTAC $ op :*= t

      Brackets (Pure b) off ->
        addTAC $ (b, off) :#= t

      Brackets (Star op) off -> do
        t1 <- newTempG
        addTAC $ t1 := B AddI op off
        addTAC $ t1 :*= t


  ICall { instP , callName, callArgs, callRetType } -> do
    comment $ "Call at " <> showP instP
    args <- mapM (either irLvalAddr irExpression) callArgs
    mapM_ (addTAC . Param) (Seq.reverse args)
    addTAC $ Call callName
    addTAC . Cleanup
      . ((+) . fromIntegral . sizeT $ callRetType)
      . (*4) . fromIntegral . length
      $ callArgs

  If { instP, ifGuards } -> do
    comment $ "If at " <> showP instP

    next <- newLabel "IfExit"
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

    whileHeader <- newLabel "WhileHeader"
    terminate $ Br whileHeader
    nextBlock <|= whileHeader

    (whileHeader #)
    next <- newLabel "WhileExit"
    irGuards next . toList $ whileGuards

    nextBlock %= tail

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

    r <- irLval readTarget
    case r of
      Single (Pure op) -> do
        addTAC $ op :<- readFunc
        addTAC $ Cleanup 0

      _ -> do
        t <- newTemp (lvalType readTarget)
        addTAC $ t :<- readFunc
        addTAC $ Cleanup 0
        case r of
          Single (Star op) ->
            addTAC $ op :*= t
          Brackets (Pure b) off ->
            addTAC $ (b, off) :#= t
          Brackets (Star b) off -> do
            t1 <- newTempG
            addTAC $ t1 := B AddI b off
            addTAC $ t1 :*= t 
          _ -> internal "The impossible happened"

  Write { instP, writeVal } -> do
    comment $ "Write at " <> showP instP

    t <- irExpression writeVal

    writeFunc <- case expType writeVal of
      Basic { atom } -> do
        let writeFunc = case atom of
              EpBoolean   -> "_writeBoolean"
              EpFloat     -> "_writeFloat"
              EpInteger   -> "_writeInteger"
              EpCharacter -> "_writeChar"
              _           -> internal "non-printable type"
        addTAC $ Param t
        pure writeFunc
      EpStr _ _     -> do
        addTAC $ RefParam t
        pure "_writeStr"
      _ -> internal "non-printable type"

    addTAC $ Call writeFunc
    addTAC $ Cleanup 4

  Make { instP, makeTarget } -> do
    comment $ "Make at " <> showP instP

    t <- newTempG
    addTAC . Param . C . IC . fromIntegral . sizeT . pointed . lvalType $ makeTarget
    addTAC $ t :<- "_make"
    addTAC $ Cleanup 4

    r <- irLval makeTarget
    case r of
      Single (Pure op) ->
        addTAC $ op := U Id t
      Single (Star op) ->
        addTAC $ op :*= t
      Brackets (Pure b) off ->
        addTAC $ (b, off) :#= t
      Brackets (Star b) off -> do
        t1 <- newTempG
        addTAC $ t1 := B AddI b off
        addTAC $ t1 :*= t

  Ekam { instP, ekamTarget } -> do
    comment $ "Ekam at " <> showP instP

    r <- irLval ekamTarget
    par <- case r of
      Single (Pure op) ->
        pure op
      Single (Star op) -> do
        t <- newTempG
        addTAC $ t :=* op
        pure t
      Brackets (Pure b) off -> do
        t <- newTempG
        addTAC $ t :=# (b, off)
        pure t
      Brackets (Star b) off -> do
        t1 <- newTempG
        t2 <- newTempG
        addTAC $ t1 := B AddI b off
        addTAC $ t2 :*= t1
        pure t2 

    addTAC $ Param par
    addTAC $ Call "_ekam"
    addTAC $ Cleanup 4

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

  Var { varName, varOffset, varSize, varType } -> do
    varName' <- insertVar varName

    let var = case varType of 
          Basic {atom = EpFloat} -> TAC.FloatVar
          _ -> TAC.Var

    addTAC $ var varName' (negate $ (fromIntegral varSize) + varOffset) varSize

irGuards :: Label -> [(Position, Expression, IBlock)] -> IRMonad ()
irGuards _ [] = internal "impossible call to irGuards"
irGuards final ((guardP, cond, iblock):gs) = do
  comment $ "Guard at " <> showP guardP

  true  <- newLabel "YesGuard"
  false <- case gs of
    [] -> pure final
    _  -> newLabel "NextGuard"
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

irRange :: Metaoperand -> [Range] -> IRMonad ()
irRange _ [] = pure ()
irRange iterator ((rangeP, low, high, iblock) : rs) = do
  comment $ "Range at " <> showP rangeP

  lOp <- irExpression low
  hOp <- irExpression high

  case iterator of
    Single (Pure op) ->
      addTAC $ op := U Id lOp
    Single (Star op) ->
      addTAC $ op :*= lOp
    Brackets (Pure b) off ->
      addTAC $ (b, off) :#= lOp
    Brackets (Star b) off -> do
      t1 <- newTempG
      addTAC $ t1 := B AddI b off
      addTAC $ t1 :*= lOp

  gHeader <- newLabel "ForHeader"
  gBody   <- newLabel "ForBody"
  next    <- newLabel "ForExit"
  terminate $ Br gHeader

  (gHeader #)

  it <- case iterator of
    Single (Pure op) ->
      pure op
    Single (Star op) -> do
      t <- newTempG
      addTAC $ t :=* op
      pure t
    Brackets (Pure b) off -> do
      t <- newTempG
      addTAC $ t :=# (b, off)
      pure t
    Brackets (Star b) off -> do
      t1 <- newTempG
      t2 <- newTempG
      addTAC $ t1 := B AddI b off 
      addTAC $ t2 :=* t1
      pure t2

  terminate $ CondBr LEI it hOp gBody next

  (gBody #)
  irIBlock iblock

  let
    one = case expType low of
      Basic { atom } | atom == EpCharacter -> C $ CC 1
                     | atom == EpInteger   -> C $ IC 1
      _ -> internal "bad type in for bounds"

  case iterator of
    Single (Pure op) ->
      addTAC $ op := B AddI op one
    Single (Star op) -> do
      t1 <- newTempG
      t2 <- newTempG
      addTAC $ t1 :=* op
      addTAC $ t2 := B AddI t1 one
      addTAC $ op :*= t2
    Brackets (Pure b) off -> do
      t1 <- newTempG
      t2 <- newTempG
      addTAC $ t1 :=# (b, off)
      addTAC $ t2 := B AddI t1 one
      addTAC $ (b, off) :#= t2
    Brackets (Star b) off -> do
      t1 <- newTempG
      t2 <- newTempG
      t3 <- newTempG
      addTAC $ t1 := B AddI b off
      addTAC $ t2 :=* t1
      addTAC $ t3 := B AddI t2 one
      addTAC $ t1 :*= t3

  terminate $ Br gHeader

  (next #)
  irRange iterator rs

irIBlock :: IBlock -> IRMonad ()
irIBlock (IBlock insts) = do
  enterScope
  mapM_ irInstruction insts
  exitScope
