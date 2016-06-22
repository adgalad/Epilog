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
    , checkCase
    , checkCaseExp
    , checkCharElem
    , checkDeclaration
    , checkFor
    , checkForD
    , checkForV
    , checkGuard
    , checkGuardCond
    , checkGuards
    , checkIf
    , checkInitialization
    , checkIntElem
    , checkRange
    , checkRangeLimits
    , checkRanges
    , checkRead
    , checkSet
    , checkSetElems
    , checkSets
    , checkSubindex
    , checkUnOp
    , checkVariable
    , checkWhile
    , checkWrite
    , declStruct
    , deref
    , expCall
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
import           Language.Epilog.At
import           Language.Epilog.Common
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Joy
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (use, (%=), (.=))
import           Control.Monad                   (forM_, when, unless)
import           Control.Monad.Trans.RWS.Strict  (asks)
import           Data.Int                        (Int32)
import           Data.List                       (find, sortOn)
import qualified Data.Map                        as Map (elems, insert,
                                                         insertWith, lookup)
import           Data.Maybe                      (fromJust)
import           Data.Sequence                   (Seq, ViewL ((:<)), (><), (|>))
import qualified Data.Sequence                   as Seq (ViewL (EmptyL),
                                                         fromList, singleton,
                                                         viewl, zipWith, index)
import           Prelude                         hiding (Either, lookup, elem)
import           Data.Foldable                   (elem)
--------------------------------------------------------------------------------
prepare :: Epilog ()
prepare = do
    procs <- asks predefinedProcs

    forM_ procs $ \proc @ Entry { eName } ->
        symbols %= insertSymbol eName proc

    ts <- asks basicTypes
    types .= ts


string :: At Token -> Epilog ()
string (TokenStringLit s :@ p) =
    strings %= Map.insertWith (flip (><)) s (Seq.singleton p)
string _ = undefined


padded :: Int -> Int -> Int
padded a o = o + (a - (o `mod` a)) `mod` a


-- Types -----------------------------------------------------------------------
lookupType :: Name -> Epilog Type
lookupType tname = do
    ts <- use types
    ctype <- use current

    if not (null ctype) && tname == item (fromJust ctype)
        then return $ Alias tname 0 0
    else case tname `Map.lookup` ts of
        Just (t, _) -> case t of
            Record _ _ s a -> return $ Alias tname s a
            Either _ _ s a -> return $ Alias tname s a
            _              -> return t
        Nothing ->
            return $ Undef tname


buildPointers :: Int -> Type -> Epilog Type
buildPointers 0 t = return t
buildPointers n t = do
    psize <- asks pointerSize
    palg  <- asks pointerAlign
    inner <- buildPointers (n-1) t
    return $ Pointer inner psize palg


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
                    when (n == sName) $ err $ RecursiveType sName n p
                _ -> return ()

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
storeProcedure' Joy { jType = instType } = do
    Just (n :@ p) <- use current
    t <- use curProcType
    symbols %= (\(Right st) -> st) . goUp
    if instType == EpVoid
        then do
            -- procInsts <- AST.topInsts
            let entry' = Entry { eName         = n
                               , eType         = t
                               , eInitialValue = Nothing
                               -- , eAST          = Just procInsts
                               , ePosition     = p
                               , eOffset       = 0
                               }
            -- AST.insert $ ProcDecl p t n procInsts
            current .= Nothing
            symbols %= insertSymbol n entry'
        else do
            let entry' = Entry { eName         = n
                               , eType         = t
                               , eInitialValue = Nothing
                               -- , eAST          = Nothing
                               , ePosition     = p
                               , eOffset       = 0
                               }
            current .= Nothing
            symbols %= insertSymbol n entry'
    symbols %= (\(Right st) -> st) . goDownLast


storeProcedure :: Type -> Epilog ()
storeProcedure t = do
    (Scope { sEntries }, _) <- use symbols
    let params = Seq.fromList . map eType . sortOn ePosition . Map.elems $
            sEntries
    curProcType .= params :-> t


-- Instructions ----------------------------------------------------------------
instructions :: Joy -> Joy -> Epilog Joy
instructions is@Joy { jType = isT, jPos } i@Joy { jType = iT } = do
    if isT == None || iT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }

---- Declaration -----------------------
checkDeclaration :: At Type -> At Name -> Epilog Joy
checkDeclaration (None :@ p) _ =
    return $ noJoy { jPos = p }
checkDeclaration (t :@ jPos) (var :@ _) = do
    symbs <- use symbols
    offs  <- use offset
    case var `local` symbs of
        Right Entry { eType, ePosition } -> do
            err $ DuplicateDeclaration var eType ePosition t jPos
            return $ noJoy { jPos }
        Left _ -> do
            symbols %= insertSymbol var
                (entry var t jPos (padded (alignT t) (head offs)))
            offset  %= \(x:xs) -> (padded (alignT t) x + sizeT t : xs)
            return $ joy { jPos }


---- Initialization --------------------
checkInitialization :: At Type -> At Name -> Joy -> Epilog Joy
checkInitialization att atn expJ = do
    Joy { jType } <- checkDeclaration att atn

    case jType of
        None   -> return $ noJoy { jPos = pos att }
        EpVoid -> return $ joy { jPos = pos att }
        _      -> undefined


---- Assign ----------------------------
checkAssign :: Joy -> Joy -> Epilog Joy
checkAssign
    Joy { jType = lvalType, jPos = p, jLval = Just lval }
    Joy { jType = eType, jExps } =
        if lvalType == eType
            then return $ joy { jPos = p }
            else case eType of
                None -> return $ noJoy { jPos = p }
                _    -> do
                    err $ AssignMismatch lvalType eType p
                    return $ noJoy { jPos = p }

    where
        theAssign = Seq.singleton $ Assign
            { instP        = p
            , assignTarget = lval
            , assignVal    = jExps `Seq.index` 0
            }


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
                return $ noJoy { jPos = p }

            Left _ -> do
                err $ UndefinedProcedure pname p
                return $ noJoy { jPos = p }

    where
        compareArgs p ets ret =
            if length js == length ets && and (Seq.zipWith eq' ets js)
                then return $ joy { jType = ret, jPos = p, jInsts = theCall }
                else if None `elem` (fmap jType js)
                    then return $ noJoy { jPos = p }
                    else  do
                        err $ BadCall pname (fmap jType js) ets p
                        return $ noJoy { jPos = p }

        eq' et Joy { jType = t } = et == t

        theCall = Seq.singleton $ ICall
            { instP    = callP
            , callName = pname
            , callArgs = fmap ((`Seq.index` 0) . jExps) js
            }


---- If --------------------------------
checkIf :: Position -> Joy -> Epilog Joy
checkIf p j@Joy { } =
    return j { jPos = p }


checkGuards :: Joy -> Joy -> Epilog Joy
checkGuards gs@Joy { jType = gsT, jPos } g@Joy { jType = gT } = do
    if gsT == None || gT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkGuard :: Joy -> Joy -> Epilog Joy
checkGuard condJ@Joy { jType = condT, jPos } instsJ@Joy { jType = instsT } = do
    if condT == None || instsT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkGuardCond :: Joy -> Epilog Joy
checkGuardCond Joy { jType = Basic { atom = EpBoolean }, jPos } =
    return $ joy { jPos }
checkGuardCond j@Joy { jType = None } =
    return j
checkGuardCond Joy { jPos } =
    return $ noJoy { jPos }


---- Case ------------------------------
checkCase :: Position -> Joy -> Joy -> Epilog Joy
checkCase p jexp@Joy { jType = eT } jsets@Joy { jType = sT } = do
    caseTypes %= tail
    if eT == None || sT == None
        then return $ noJoy { jPos = p }
        else return $ joy { jPos = p }


checkCaseExp :: Joy -> Epilog Joy
checkCaseExp j@Joy { jType, jPos } = do
    if jType `elem` ([intT, charT] :: [Type])
        then do
            caseTypes %= ((jType :@ jPos) :)
            return j
        else do
            caseTypes %= ((None :@ jPos) :)
            err $ BadCaseExp jType jPos
            return $ noJoy { jPos }


checkSets :: Joy -> Joy -> Epilog Joy
checkSets ss@Joy { jType = ssT, jPos } s@Joy { jType = sT } = do
    if ssT == None || sT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkSet :: Joy -> Joy -> Epilog Joy
checkSet elemsJ@Joy { jType = elemsT, jPos } instsJ@Joy { jType = instsT } = do
    if elemsT == None || instsT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkSetElems :: Joy -> Joy -> Epilog Joy
checkSetElems elsJ@Joy { jType = elsT, jPos } elJ@Joy { jType = elT } = do
    if elsT == None || elT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkIntElem :: At Int32 -> Epilog Joy
checkIntElem (el :@ elp) = do
    ((ct :@ p):_) <- use caseTypes
    if ct == intT
        then do
            -- caseSet %= (|> LitInt elp el)
            return $ joy { jType = intT, jPos = elp }
        else do
            err $ BadCaseCharElem p el elp
            return $ noJoy { jPos = elp }


checkCharElem :: At Char -> Epilog Joy
checkCharElem (el :@ elp) = do
    ((ct :@ p):_) <- use caseTypes
    if (ct == charT)
        then do
            -- caseSet %= (|> LitChar elp el)
            return $ joy { jType = charT, jPos = elp }
        else do
            err $ BadCaseIntElem p el elp
            return $ noJoy { jPos = elp }


---- For -------------------------------
checkFor :: Position -> Joy -> Joy -> Epilog Joy
checkFor p jit@Joy { jType = itT } jrngs@Joy { jType = rngsT } = do
    forVars %= tail
    if itT == None || rngsT == None
        then return $ noJoy { jPos = p }
        else return $ joy { jPos = p }


checkForD :: At Type -> At Name -> Epilog Joy
checkForD att@(t :@ p) var@(n :@ _) =
    if t `elem` ([intT, charT] :: [Type])
        then do
            Joy { jType = t' } <- checkDeclaration att var
            case t' of
                None -> do
                    forVars %= ((var, None):)
                    return $ noJoy { jPos = p }
                _    -> do
                    forVars %= ((var, t):)
                    return $ joy { jPos = p }
        else do
            checkDeclaration (None :@ p) var
            err $ BadForVar n t p p
            forVars %= ((var, None):)
            return $ noJoy { jPos = p }


checkForV :: At Name -> Epilog Joy
checkForV var@(n :@ p) = do
    Joy { jType = t, jPos = pdec } <- checkVariable var
    if t `elem` ([intT, charT] :: [Type])
        then do
            forVars %= ((var, t):)
            return $ joy { jPos = pdec }
        else do
            err $ BadForVar n t pdec p
            forVars %= ((var, None):)
            return $ noJoy { jPos = pdec }

checkRanges :: Joy -> Joy -> Epilog Joy
checkRanges rs@Joy { jType = rsT, jPos } r@Joy { jType = rT } = do
    if rsT == None || rT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkRange :: Joy -> Joy -> Epilog Joy
checkRange r@Joy { jType = rT, jPos } insts@Joy { jType = instsT } = do
    if rT == None || instsT == None
        then return $ noJoy { jPos }
        else return $ joy { jPos }


checkRangeLimits :: Joy -> Joy -> Epilog Joy
checkRangeLimits Joy { jType = t1, jPos } Joy { jType = t2 } = do
    ((n :@ vp, t):_) <- use forVars
    if t1 == t2 && t1 == t
        then return $ joy { jPos }
        else do
            err $ InvalidRange n t vp t1 t2 jPos
            return $ noJoy { jPos }


---- While -----------------------------
checkWhile :: Position -> Joy -> Epilog Joy
checkWhile p j@Joy { } =
    return j { jPos = p }


---- Finish & Answer -------------------
checkAnswer :: Position -> Joy -> Epilog Joy
checkAnswer retp Joy { jType = None } = return $ noJoy { jPos = retp }
checkAnswer retp Joy { jType = aret, jExps } = do
    Just proc <- use current
    Joy { jType = _ :-> eret, jPos = procp } <- checkVariable proc

    if eret == aret
        then return $ joy { jPos = retp, jInsts = theAnswer }
        else do
            if aret == EpVoid
                then err $ BadFinish eret      procp retp
                else err $ BadAnswer eret aret procp retp
            return $ noJoy { jPos = retp }

    where
        theAnswer = Seq.singleton $ if aret == EpVoid
            then Finish { instP = retp }
            else Answer
                { instP     = retp
                , answerVal = jExps `Seq.index` 0
                }


---- Write -----------------------------
checkWrite :: Position -> Joy -> Epilog Joy
checkWrite p Joy { jType = None } = return $ noJoy { jPos = p }
checkWrite p Joy { jType = t, jExps } =
    if t `elem` ([boolT, charT, intT, floatT, stringT] :: [Type])
        then return $ joy { jPos = p, jInsts = theWrite }
        else do
            unless (t == None) $ err $ BadWrite t p
            return $ noJoy { jPos = p }

    where
        theWrite = Seq.singleton $ Write
            { instP    = p
            , writeVal = jExps `Seq.index` 0
            }


---- Read ------------------------------
checkRead :: Position -> Joy -> Epilog Joy
checkRead p Joy { jType = None } = return $ noJoy { jPos = p }
checkRead p Joy { jType = t, jLval = Just lval } =
    if t `elem` ([boolT, charT, intT, floatT] :: [Type])
        then return $ joy { jPos = p }
        else do
            err $ BadRead t p
            return $ noJoy { jPos = p }
    where
        theRead = Seq.singleton $ Read
            { instP      = p
            , readTarget = lval
            }

checkRead _ _ = undefined

-- Lvals -----------------------------------------------------------------------
checkVariable :: At Name -> Epilog Joy
checkVariable (name :@ p) = do
    symbs <- use symbols
    case name `lookup` symbs of
        Right (Entry { eType }) ->
            return $ joy { jType = eType, jPos = p, jLval = Just theLval }
        Left _ -> do
            err $ OutOfScope name p
            return $ noJoy { jPos = Position 0 0 }

    where
        theLval = Variable name


checkSubindex :: Joy -> Joy -> Epilog Joy
checkSubindex
    Joy { jType = Array { inner } , jPos, jLval = Just lval }
    Joy { jType = indexT, jExps } =
        if indexT == intT
            then return $ joy { jType = inner, jPos, jLval = Just theLval }
            else do
                err $ InvalidSubindex indexT jPos
                return $ noJoy { jPos }
    where
        theLval = Index lval (jExps `Seq.index` 0)

checkSubindex j @ Joy { jType = None } _ =
    return j
checkSubindex Joy { jPos } _ = do
    err $ InvalidArray jPos
    return noJoy { jPos }


getField :: Joy -> At Name -> Epilog Joy
getField Joy { jType, jLval = Just lval } (fieldname :@ p) = case jType of
    Alias tname _ _ -> do
        ts <- use types
        case tname `Map.lookup` ts of
            Just (Record { fields  }, _) -> checkField  fields
            Just (Either { members }, _) -> checkMember members
            _                         -> err' InvalidAccess
    _  -> err' InvalidAccess

    where
        checkField fields = case fieldname `Map.lookup` fields of
            Just t'  -> return $
                joy { jType = fst t', jPos = p, jLval = Just theLval }
            Nothing -> err' InvalidField
        checkMember members = case fieldname `Map.lookup` members of
            Just t'  -> return $
                joy { jType = fst t', jPos = p, jLval = Just theLval }
            Nothing -> err' InvalidMember
        theLval = Member lval fieldname
        err' cons = do
            err $ cons fieldname (name jType) p
            return $ noJoy { jPos = p }


deref :: Joy -> Epilog Joy
deref Joy { jType = Pointer { pointed }, jPos, jLval = Just lval } =
    return $ joy { jType = pointed, jPos, jLval = Just (Deref lval) }
deref j @ Joy { jType = None } =
    return j
deref Joy { jType, jPos } = do
    err $ BadDeref jType jPos
    return $ noJoy { jPos }


-- Expressions -----------------------------------------------------------------
---- Call Expression -------------------
expCall :: Joy -> Epilog Joy
expCall Joy { jType, jPos, jInsts } =
    return $ joy { jType, jPos, jExps = theExpCall }

    where
        theExpCall = Seq.singleton $ ECall instP callName callArgs
        ICall { instP, callName, callArgs } :< _ = Seq.viewl jInsts

---- Lval Expression -------------------
expLval :: Joy -> Epilog Joy
expLval Joy { jType, jPos, jLval = Just lval } =
    return $ joy { jType, jPos, jExps = theExpLval }

    where
        theExpLval = Seq.singleton $ Lval jPos lval


---- Binary Expressions ----------------
checkBinOp :: Position -> BinaryOp -> Joy -> Joy -> Epilog Joy
checkBinOp p op = aux opTypes
    where
        aux _ Joy { jType = None } _ = return noJoy { jPos = p }
        aux _ _ Joy { jType = None } = return noJoy { jPos = p }
        aux [] Joy { jType = t1 } Joy { jType = t2 } = do
            err $ BadBinaryExpression op (t1, t2) (domain opTypes) p
            return $ noJoy { jPos = p }
        aux ((et1, et2, rt) : ts) j1@Joy { jType = t1 } j2@Joy { jType = t2 } =
            if t1 == et1 && t2 == et2
                then return $ joy
                    { jType = rt, jPos = p, jExps = theBinExp }
                else aux ts j1 j2
            where
                theBinExp = Seq.singleton $
                    Binary p op
                        (jExps j1 `Seq.index` 0)
                        (jExps j2 `Seq.index` 0)
        opTypes = typeBinOp op
        domain = map (\(a, b, _) -> (a, b))



---- Unary Expressions -----------------
checkUnOp :: Position -> UnaryOp -> Joy -> Epilog Joy
checkUnOp p op = aux opTypes
    where
        aux _ Joy { jType = None } = return $ noJoy { jPos = p }
        aux [] Joy { jType = t1 } = do
            err $ BadUnaryExpression op t1 (domain opTypes) p
            return $ noJoy { jPos = p }
        aux ((et, rt) : ts) j@Joy { jType = t } =
            if t == et
                then return $ joy
                    { jType = rt, jPos = p, jExps = theUnExp }
                else aux ts j
            where
                theUnExp = Seq.singleton $
                    Unary p op (jExps j `Seq.index` 0)
        opTypes = typeUnOp op
        domain = map fst



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
