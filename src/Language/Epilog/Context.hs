{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( buildArray
    , buildPointers
    , checkAnswer
    , checkAssign
    , checkBinOp
    , checkCall
    -- , checkCase
    -- , checkCaseExp
    -- , checkCharElem
    , checkDeclaration
    , checkFor
    -- , checkForD
    , checkForV
    , checkGuard
    , checkGuardCond
    , checkGuards
    , checkIf
    , checkInitialization
    -- , checkIntElem
    , checkParam
    , checkRange
    , checkRangeLimits
    , checkRanges
    , checkRead
    -- , checkSet
    -- , checkSetElems
    -- , checkSets
    , checkSubindex
    , checkUnOp
    , checkVariable
    , checkWhile
    , checkWrite
    , declStruct
    , deref
    , expCall
    , expLval
    , getField
    , instructions
    , lookupType
    , prepare
    , storeProcedure
    , storeProcedure'
    , string
    , verifyField
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Procedure
import           Language.Epilog.At
import           Language.Epilog.Common
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Joy
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (at, use, (%%=), (%=), (&),
                                                  (.=), (?=), (?~), (|>))
import           Control.Monad                   (forM_, unless, when)
import           Control.Monad.Reader            (asks)
import           Data.Foldable                   (elem)
import           Data.List                       (find, uncons)
import qualified Data.Map                        as Map (insert, lookup, size)
import           Data.Sequence                   (ViewL ((:<)))
import qualified Data.Sequence                   as Seq (ViewL (EmptyL), empty,
                                                         singleton, viewl,
                                                         zipWith)
import           Prelude                         hiding (Either, elem, lookup)
--------------------------------------------------------------------------------

prepare :: Epilog ()
prepare = do
    procs <- asks predefinedProcs

    forM_ procs $ \proc @ Entry { eName } ->
        symbols %= insertSymbol eName proc

    ts <- asks basicTypes
    types .= ts


string :: At Token -> Epilog Int32
string (TokenStringLit s :@ _) =
  strings %%= \m -> case s `Map.lookup` m of
    Just i  -> (i, m)
    Nothing -> let i = fromIntegral (Map.size m) in (i, m & at s ?~ i)
string _ = undefined


padded :: Int -> Int -> Int
padded a o = o + (a - (o `mod` a)) `mod` a


-- Types -----------------------------------------------------------------------
lookupType :: Name -> Epilog Type
lookupType tname = do
    ts <- use types
    ctype <- use current

    if not (null ctype) && tname == item (fromJust ctype)
        then pure $ Alias tname 0 0
    else case tname `Map.lookup` ts of
        Just (t, _) -> case t of
            Record _ _ s a -> pure $ Alias tname s a
            Either _ _ s a -> pure $ Alias tname s a
            _              -> pure t
        Nothing ->
            pure $ Undef tname


buildPointers :: Int -> Type -> Epilog Type
buildPointers 0 t = pure t
buildPointers n t = do
    psize <- asks pointerSize
    palg  <- asks pointerAlign
    inner <- buildPointers (n-1) t
    pure $ Pointer inner psize palg


buildArray :: Seq (Int32, Int32) -> Type -> Type
buildArray _ x@(Undef _) = x
buildArray sizes t = case Seq.viewl sizes of
    Seq.EmptyL         -> t
    (low, high) :< lhs -> Array low high innerT size' al'

        where
            innerT = buildArray lhs t
            al'    = alignT innerT
            size'  = sizeT  innerT * fromIntegral (high - low + 1)


-- Structs ---------------------------------------------------------------------
declStruct :: Epilog ()
declStruct = do
    Just (sName :@ p) <- use current
    ts <- use types
    al <- use structAlign
    sz <- use structSize

    case sName `Map.lookup` ts of
        Just (_, p0) ->
            err $ DuplicateDefinition sName p0 p
        Nothing -> do
            Just structK <- use curkind
            let struct = toCons structK
            fs <- use curfields
            forM_ fs $
                check sName
            case structK of
                EitherK -> do
                    let sz' = padded al sz
                    types %= Map.insert sName
                        (struct sName (toMap fs) sz' al, p)

                RecordK ->
                    types %= Map.insert sName
                        (struct sName (toMap fs) sz al, p)

    current   .= Nothing
    curkind   .= Nothing
    curfields .= []
    offset    %= tail

    where
        check sName (name :@ p, t, _) =
            case t of
                Array { inner } -> check sName (name :@ p, inner, 0)
                Alias { name = n } ->
                    when (n == sName) . err $ RecursiveType sName n p
                _ -> pure ()

        toMap                    = foldr toMap' []
        toMap' (name :@ _, t, o) = Map.insert name (t, o)


verifyField :: At Name -> Type -> Epilog ()
verifyField f@(n :@ p) t = do
    cf     <- use curfields
    Just k <- use curkind

    case find sameName cf of
        Just (_ :@ p1, t1, _) -> do
            Just (sName :@ sPos) <- use current
            err $ DuplicateField sName k sPos n t1 p1 t p
        Nothing -> do
            (o:os) <- use offset
            curfields %= (|> (f, t, o))
            case k of
                RecordK -> do
                    let newOffset = padded (alignT t) o + sizeT t
                    offset       .= newOffset : os
                    structAlign  %= max (alignT t)
                    structSize   .= newOffset
                EitherK -> do
                    structAlign %= max (alignT t)
                    structSize  %= max (sizeT t)
    where
        sameName (n1 :@ _,_,_) = n == n1


-- Procedures ------------------------------------------------------------------
storeProcedure' :: Joy -> Epilog ()
storeProcedure' Joy { jType = blockType, jInsts } = do
    Just (n :@ p) <- use current
    t <- use curProcType

    scope <- symbols %%= extractScope

    if blockType == None
      then
        procedures . at n ?= Procedure
          { procName   = n
          , procPos    = p
          , procType   = t
          , procParams = Seq.empty
          , procDef    = Nothing }

      else do
        params <- use parameters

        procedures . at n ?= Procedure
          { procName   = n
          , procPos    = p
          , procType   = t
          , procParams = params
          , procDef    = Just (jInsts, scope) }

    current .= Nothing
    curProcType .= None
    parameters .= Seq.empty


storeProcedure :: Type -> Epilog ()
storeProcedure t = do
  params <- fmap parType <$> use parameters
  curProcType .= params :-> t


-- Instructions ----------------------------------------------------------------
instructions :: Joy -> Joy -> Epilog Joy
instructions
    Joy { jType = isT, jPos, jInsts = is }
    Joy { jType = iT, jInsts = i }
    = if isT == None || iT == None
        then pure $ noJoy { jPos }
        else pure $ joy { jPos, jInsts = is <> i }

---- Parameter -------------------------
checkParam :: At Type -> At Name -> Epilog Joy
checkParam att@(parType :@ _) atn@(parName :@ parPos) = do
  j@Joy { jType } <- checkDeclaration att atn
  case jType of
    None -> pure j
    _ -> do
      parameters |>= Parameter
        { parName
        , parType
        , parPos }
      pure j


---- Initialization and Declaration ----
checkDeclaration :: At Type -> At Name -> Epilog Joy
checkDeclaration att atn = checkInitialization' att atn Nothing

checkInitialization :: At Type -> At Name -> Joy -> Epilog Joy
checkInitialization att atn = checkInitialization' att atn . Just

checkInitialization' :: At Type -> At Name -> Maybe Joy -> Epilog Joy
checkInitialization' (None :@ p) _ _ =
  pure $ noJoy { jPos = p }
checkInitialization' (t :@ _) (var :@ jPos) mj = do
  symbs <- use symbols
  offs  <- use offset
  eKind <- use entryKind
  case var `local` symbs of
    Right Entry { eType, ePosition } -> do
      err $ DuplicateDeclaration var eType ePosition t jPos
      pure $ noJoy { jPos }
    Left _ ->
      case mj of
        Nothing -> do
          symbols %= insertSymbol var Entry
            { eName         = var
            , eKind
            , eType         = t
            , ePosition     = jPos
            , eInitialValue = Nothing
            , eOffset       = padded (alignT t) (head offs)
            , eOperand      = Nothing }
          offset  %= \(x:xs) -> (padded (alignT t) x + sizeT t : xs)
          pure joy { jPos }
        Just Joy { jType, jExp }
          | jType == None -> pure noJoy { jPos }
          | jType == t    -> do
            symbols %= insertSymbol var Entry
              { eName         = var
              , eKind
              , eType         = t
              , ePosition     = jPos
              , eInitialValue = jExp
              , eOffset       = padded (alignT t) (head offs)
              , eOperand      = Nothing }
            offset  %= \(x:xs) -> (padded (alignT t) x + sizeT t : xs)
            pure joy { jPos }
          | otherwise    -> do
            err AssignMismatch { amFstT = t, amSndT = jType, amP = jPos}
            pure noJoy { jPos }

---- Assign ----------------------------
checkAssign :: Joy -> Joy -> Epilog Joy
checkAssign
  Joy { jType = lvalType, jPos = p, jLval }
  Joy { jType = eType, jExp } =
    if lvalType == eType
      then pure $ joy { jPos = p, jInsts = theAssign }
      else case eType of
        None -> pure $ noJoy { jPos = p }
        _    -> do
          err $ AssignMismatch lvalType eType p
          pure $ noJoy { jPos = p }

  where
    theAssign = Seq.singleton Assign
      { instP        = p
      , assignTarget = fromJust jLval
      , assignVal    = fromJust jExp }

---- Call ------------------------------
checkCall :: At Name -> Seq Joy -> Epilog Joy
checkCall (pname :@ callP) js = do
  symbs <- use symbols
  Just (n :@ p) <- use current
  (ets :-> ret) <- use curProcType
  if pname == n
    then compareArgs p ets ret
    else case pname `lookup` symbs of
      Right Entry { eType = ets' :-> ret' } ->
        compareArgs p ets' ret'

      Right _ -> do
        err $ UndefinedProcedure pname p
        pure $ noJoy { jPos = p }

      Left _ -> do
        err $ UndefinedProcedure pname p
        pure $ noJoy { jPos = p }

  where
    compareArgs p ets ret
      | length js == length ets && and (Seq.zipWith eq' ets js) =
        pure $ joy { jType = ret, jPos = p, jInsts = theCall }
      | None `elem` fmap jType js = pure $ noJoy { jPos = p }
      | otherwise = do
        err $ BadCall pname (fmap jType js) ets p
        pure $ noJoy { jPos = p }

    eq' et Joy { jType = t } = et == t

    theCall = Seq.singleton ICall
      { instP    = callP
      , callName = pname
      , callArgs = fromJust . jExp <$> js }


---- If --------------------------------
checkIf :: Position -> Joy -> Epilog Joy
checkIf p Joy { jType, jGuards }
  | jType == None = pure noJoy { jPos = p }
  | otherwise = pure joy
    { jPos = p
    , jInsts = Seq.singleton If
      { instP = p
      , ifGuards = jGuards } }


checkGuards :: Joy -> Joy -> Epilog Joy
checkGuards
  Joy { jType = gsT, jPos, jGuards }
  Joy { jType = gT, jGuards = jGuard }
  | gsT == None || gT == None = pure $ noJoy { jPos }
  | otherwise = pure $ joy
    { jPos
    , jGuards = jGuards <> jGuard }

checkGuard :: Joy -> Joy -> Epilog Joy
checkGuard
  Joy { jType = condT, jPos, jExp }
  Joy { jType = instsT, jInsts }
  | condT == None || instsT == None = pure noJoy { jPos }
  | otherwise = pure joy
    { jPos
    , jGuards = Seq.singleton
      ( jPos
      , fromJust jExp
      , jInsts ) }

checkGuardCond :: Joy -> Epilog Joy
checkGuardCond j@Joy { jType = Basic { atom = EpBoolean } } =
  pure j
checkGuardCond j@Joy { jType = None } =
  pure j
checkGuardCond Joy { jPos, jType } = do
  err InvalidGuard { igT = jType, igP = jPos }
  pure noJoy { jPos }


-- ---- Case ------------------------------
-- checkCase :: Position -> Joy -> Joy -> Epilog Joy
-- checkCase p jexp@Joy { jType = eT } jsets@Joy { jType = sT } = do
--     caseTypes %= tail
--     if eT == None || sT == None
--         then pure $ noJoy { jPos = p }
--         else pure $ joy { jPos = p }
--
--
-- checkCaseExp :: Joy -> Epilog Joy
-- checkCaseExp j@Joy { jType, jPos } =
--   if jType `elem` ([intT, charT] :: [Type])
--     then do
--       caseTypes %= ((jType :@ jPos) :)
--       pure j
--     else do
--       caseTypes %= ((None :@ jPos) :)
--       err $ BadCaseExp jType jPos
--       pure $ noJoy { jPos }
--
--
-- checkSets :: Joy -> Joy -> Epilog Joy
-- checkSets ss@Joy { jType = ssT, jPos } s@Joy { jType = sT } = do
--     if ssT == None || sT == None
--         then pure $ noJoy { jPos }
--         else pure $ joy { jPos }
--
--
-- checkSet :: Joy -> Joy -> Epilog Joy
-- checkSet elemsJ@Joy { jType = elemsT, jPos } instsJ@Joy { jType = instsT } = do
--     if elemsT == None || instsT == None
--         then pure $ noJoy { jPos }
--         else pure $ joy { jPos }
--
--
-- checkSetElems :: Joy -> Joy -> Epilog Joy
-- checkSetElems elsJ@Joy { jType = elsT, jPos } elJ@Joy { jType = elT } = do
--     if elsT == None || elT == None
--         then pure $ noJoy { jPos }
--         else pure $ joy { jPos }
--
--
-- checkIntElem :: At Int32 -> Epilog Joy
-- checkIntElem (el :@ elp) = do
--     ((ct :@ p):_) <- use caseTypes
--     if ct == intT
--         then do
--             -- caseSet %= (|> LitInt elp el)
--             pure $ joy { jType = intT, jPos = elp }
--         else do
--             err $ BadCaseCharElem p el elp
--             pure $ noJoy { jPos = elp }
--
--
-- checkCharElem :: At Char -> Epilog Joy
-- checkCharElem (el :@ elp) = do
--     ((ct :@ p):_) <- use caseTypes
--     if (ct == charT)
--         then do
--             -- caseSet %= (|> LitChar elp el)
--             pure $ joy { jType = charT, jPos = elp }
--         else do
--             err $ BadCaseIntElem p el elp
--             pure $ noJoy { jPos = elp }


---- For -------------------------------
checkFor :: Position -> Joy -> Joy -> Epilog Joy
checkFor p Joy { jType = itT } Joy { jType = rngsT, jRanges }
  | itT == None || rngsT == None = do
    forVars %= tail
    pure noJoy { jPos = p }
  | otherwise = do
    (var :@ _, _) <- forVars %%= fromJust . uncons
    pure joy
      { jPos = p
      , jInsts = Seq.singleton For
        { instP     = p
        , forVar    = var
        , forRanges = jRanges } }


-- checkForD :: At Type -> At Name -> Epilog Joy
-- checkForD att@(t :@ p) var@(n :@ _) =
--     if t `elem` ([intT, charT] :: [Type])
--         then do
--             Joy { jType = t' } <- checkDeclaration att var
--             case t' of
--                 None -> do
--                     forVars %= ((var, None):)
--                     pure $ noJoy { jPos = p }
--                 _    -> do
--                     forVars %= ((var, t):)
--                     pure $ joy { jPos = p }
--         else do
--             checkDeclaration (None :@ p) var
--             err $ BadForVar n t p p
--             forVars %= ((var, None):)
--             pure $ noJoy { jPos = p }


checkForV :: At Name -> Epilog Joy
checkForV var@(n :@ p) = do
  Joy { jType = t, jPos = pdec } <- checkVariable var
  if t `elem` ([intT, charT] :: [Type])
    then do
      forVars %= ((var, t):)
      pure $ joy { jPos = pdec }
    else do
      err $ BadForVar n t pdec p
      forVars %= ((var, None):)
      pure $ noJoy { jPos = pdec }


checkRanges :: Joy -> Joy -> Epilog Joy
checkRanges Joy { jType = rsT, jPos, jRanges } Joy { jType = rT, jRanges = jRange }
  | rsT == None || rT == None = pure noJoy { jPos }
  | otherwise = pure $ joy
    { jPos
    , jRanges = jRanges <> jRange }


checkRange :: Joy -> Joy -> Epilog Joy
checkRange Joy { jType = rT, jPos, jExp, jExp' } Joy { jType = instsT, jInsts }
  | rT == None || instsT == None = pure noJoy { jPos }
  | otherwise = pure $ joy
    { jPos
    , jRanges = Seq.singleton
      ( jPos
      , fromJust jExp
      , fromJust jExp'
      , jInsts ) }


checkRangeLimits :: Joy -> Joy -> Epilog Joy
checkRangeLimits Joy { jType = t1, jPos, jExp } Joy { jType = t2, jExp = jExp' } = do
  ((n :@ vp, t):_) <- use forVars
  if t1 == t2 && t1 == t
    then pure $ joy { jPos, jExp, jExp' }
    else do
      err $ InvalidRange n t vp t1 t2 jPos
      pure $ noJoy { jPos }


---- While -----------------------------
checkWhile :: Position -> Joy -> Epilog Joy
checkWhile p Joy { jType, jGuards }
  | jType == None = pure noJoy { jPos = p }
  | otherwise = pure joy
    { jPos = p
    , jInsts = Seq.singleton While
      { instP = p
      , whileGuards = jGuards } }


---- Finish & Answer -------------------
checkAnswer :: Position -> Joy -> Epilog Joy
checkAnswer retp Joy { jType = None } = pure noJoy { jPos = retp }
checkAnswer retp Joy { jType = aret, jExp } = do
  Just (_ :@ procp) <- use current
  _ :-> eret <- use curProcType

  if eret == aret
    then pure joy { jPos = retp, jInsts = theAnswer }
    else do
      if aret == EpVoid
        then err $ BadFinish eret      procp retp
        else err $ BadAnswer eret aret procp retp
      pure noJoy { jPos = retp }

  where
    theAnswer = Seq.singleton $ if aret == EpVoid
      then Finish { instP = retp }
      else Answer
        { instP     = retp
        , answerVal = fromJust jExp }


---- Write -----------------------------
checkWrite :: Position -> Joy -> Epilog Joy
checkWrite p Joy { jType = None } = pure $ noJoy { jPos = p }
checkWrite p Joy { jType = t, jExp } =
    if t `elem` ([boolT, charT, intT, floatT, stringT] :: [Type])
        then pure $ joy { jPos = p, jInsts = theWrite }
        else do
            unless (t == None) . err $ BadWrite t p
            pure $ noJoy { jPos = p }

    where
        theWrite = Seq.singleton Write
            { instP    = p
            , writeVal = fromJust jExp
            }

---- Read ------------------------------
checkRead :: Position -> Joy -> Epilog Joy
checkRead p Joy { jType = None } = pure $ noJoy { jPos = p }
checkRead p Joy { jType = t, jLval = Just lval } =
  if t `elem` ([boolT, charT, intT, floatT] :: [Type])
    then pure $ joy { jPos = p, jInsts = theRead }
    else do
      err $ BadRead t p
      pure $ noJoy { jPos = p }
  where
    theRead = Seq.singleton Read
      { instP      = p
      , readTarget = lval }

checkRead _ _ = error "internal error: non-none read without lval"

-- Lvals -----------------------------------------------------------------------
checkVariable :: At Name -> Epilog Joy
checkVariable (name :@ p) = do
    symbs <- use symbols
    case name `lookup` symbs of
        Right Entry { eType } ->
            pure $ joy { jType = eType, jPos = p, jLval = Just theLval }
        Left _ -> do
            err $ OutOfScope name p
            pure $ noJoy { jPos = Position 0 0 }

    where
        theLval = Variable name


checkSubindex :: Joy -> Joy -> Epilog Joy
checkSubindex
    Joy { jType = Array { inner } , jPos, jLval = Just lval }
    Joy { jType = indexT, jExp } =
        if indexT == intT
            then pure $ joy { jType = inner, jPos, jLval = Just theLval }
            else do
                err $ InvalidSubindex indexT jPos
                pure $ noJoy { jPos }
    where
        theLval = Index lval . fromJust $ jExp

checkSubindex j @ Joy { jType = None } _ =
    pure j
checkSubindex Joy { jPos } _ = do
    err $ InvalidArray jPos
    pure noJoy { jPos }


getField :: Joy -> At Name -> Epilog Joy
getField
  Joy { jPos, jType, jLval }
  (fieldname :@ p)
  = case jType of
    Alias tname _ _ -> do
      ts <- use types
      case tname `Map.lookup` ts of
        Just (Record { fields  }, _) -> checkField  fields
        Just (Either { members }, _) -> checkMember members
        _                            -> err' InvalidAccess
    None -> pure noJoy { jPos }
    _  -> err' InvalidAccess

  where
    checkField fields = case fieldname `Map.lookup` fields of
      Just t'  -> pure $
        joy { jType = fst t', jPos = p, jLval = Just theLval }
      Nothing -> err' InvalidField
    checkMember members = case fieldname `Map.lookup` members of
      Just t'  -> pure $
        joy { jType = fst t', jPos = p, jLval = Just theLval }
      Nothing -> err' InvalidMember
    theLval = Member (fromJust jLval) fieldname
    err' cons = do
      err $ cons fieldname (name jType) p
      pure $ noJoy { jPos = p }


deref :: Joy -> Epilog Joy
deref Joy { jType = Pointer { pointed }, jPos, jLval = Just lval } =
    pure $ joy { jType = pointed, jPos, jLval = Just (Deref lval) }
deref j @ Joy { jType = None } =
    pure j
deref Joy { jType, jPos } = do
    err $ BadDeref jType jPos
    pure $ noJoy { jPos }


-- Expressions -----------------------------------------------------------------
---- Call Expression -------------------
expCall :: Joy -> Epilog Joy
expCall Joy { jType, jPos, jInsts } =
    pure $ joy { jType, jPos, jExp = theExpCall }

    where
        theExpCall = Just Expression
          { expPos = instP
          , expType = jType
          , exp' = ECall callName callArgs }
        ICall { instP, callName, callArgs } :< _ = Seq.viewl jInsts

---- Lval Expression -------------------
expLval :: Joy -> Epilog Joy
expLval Joy { jType, jPos, jLval }
  | jType == None = pure noJoy { jPos }
  | otherwise     = pure joy { jType, jPos, jExp = theExpLval }

  where
    theExpLval = Just Expression
      { expPos = jPos
      , expType = jType
      , exp' = Lval (fromJust jLval) }


---- Binary Expressions ----------------
checkBinOp :: Position -> BinaryOp -> Joy -> Joy -> Epilog Joy
checkBinOp p op = aux opTypes
  where
    aux _ Joy { jType = None } _ = pure noJoy { jPos = p }
    aux _ _ Joy { jType = None } = pure noJoy { jPos = p }
    aux [] Joy { jType = t1 } Joy { jType = t2 } = do
      err $ BadBinaryExpression op (t1, t2) (domain opTypes) p
      pure $ noJoy { jPos = p }
    aux ((et1, et2, rt) : ts) j1@Joy { jType = t1 } j2@Joy { jType = t2 } =
      if t1 == et1 && t2 == et2
        then pure $ joy
          { jType = rt, jPos = p, jExp = theBinExp }
        else aux ts j1 j2
      where
        theBinExp = Just Expression
          { expPos = p
          , expType = rt
          , exp' = Binary op (fromJust $ jExp j1) (fromJust $ jExp j2) }
    aux _ _ _ = error "internal error: compiler complained about missing cases"
    opTypes = typeBinOp op
    domain = fmap (\(a, b, _) -> (a, b))


---- Unary Expressions -----------------
checkUnOp :: Position -> UnaryOp -> Joy -> Epilog Joy
checkUnOp p op = aux opTypes
  where
    aux _ Joy { jType = None } = pure $ noJoy { jPos = p }
    aux [] Joy { jType = t1 } = do
      err $ BadUnaryExpression op t1 (domain opTypes) p
      pure $ noJoy { jPos = p }
    aux ((et, rt) : ts) j@Joy { jType = t, jExp } =
      if t == et
        then pure $ joy
          { jType = rt, jPos = p, jExp = theUnExp }
        else aux ts j
      where
        theUnExp = Just Expression
          { expPos = p
          , expType = rt
          , exp' = Unary op (fromJust jExp ) }
    aux _ _ = error "internal error: compiler complained about missing cases"
    opTypes = typeUnOp op
    domain = fmap fst



---- Expression Types ------------------
typeBinOp :: BinaryOp -> [(Type, Type, Type)]
typeBinOp And      = [ ( boolT,  boolT,  boolT  ) ]
typeBinOp Andalso  = [ ( boolT,  boolT,  boolT  ) ]
typeBinOp Or       = [ ( boolT,  boolT,  boolT  ) ]
typeBinOp Orelse   = [ ( boolT,  boolT,  boolT  ) ]
typeBinOp Xor      = [ ( boolT,  boolT,  boolT  ) ]
typeBinOp Band     = [ ( intT,   intT,   intT   ) ]
typeBinOp Bor      = [ ( intT,   intT,   intT   ) ]
typeBinOp Bsl      = [ ( intT,   intT,   intT   ) ]
typeBinOp Bsr      = [ ( intT,   intT,   intT   ) ]
typeBinOp Bxor     = [ ( intT,   intT,   intT   ) ]
typeBinOp Plus     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT ) ]
typeBinOp Minus    = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT ) ]
typeBinOp Times    = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT ) ]
typeBinOp FloatDiv = [ ( floatT, floatT, floatT ) ]
typeBinOp IntDiv   = [ ( intT,   intT,   intT   ) ]
typeBinOp Rem      = [ ( intT,   intT,   intT   ) ]
typeBinOp LTop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  ) ]
typeBinOp LEop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  ) ]
typeBinOp GTop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  ) ]
typeBinOp GEop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  ) ]
typeBinOp EQop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  )
                     , ( boolT,  boolT,  boolT  ) ]
typeBinOp NEop     = [ ( intT,   intT,   boolT  )
                     , ( floatT, floatT, boolT  )
                     , ( charT,  charT,  boolT  )
                     , ( boolT,  boolT,  boolT  ) ]
typeBinOp FAop     = [ ( intT,   intT,   boolT  ) ]
typeBinOp NFop     = [ ( intT,   intT,   boolT  ) ]


typeUnOp :: UnaryOp -> [(Type, Type)]
typeUnOp Not    = [ ( boolT,  boolT  ) ]
typeUnOp Bnot   = [ ( intT,   intT   ) ]
typeUnOp Uminus = [ ( intT,   intT   )
                  , ( floatT, floatT ) ]
