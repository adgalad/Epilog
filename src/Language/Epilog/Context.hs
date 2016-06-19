{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( arrayPadding
    , buildArray
    , buildPointers
    , checkAnswer
    , checkArray
    , checkAssign
    , checkBinOp
    , checkBoth
    , checkCall
    , checkDeclVar
    , checkFor
    , checkInit
    , checkRead
    , checkUnOp
    , checkWrite
    , declStruct
    , deref
    , findType
    , findTypeOfSymbol
    , getField
    , isSymbol'
    , storeProcedure
    , string
    , verifyField
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.At
import           Language.Epilog.Common
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (%=), (.=))
import           Control.Monad                  (forM_, unless, when)
import           Data.Int                       (Int32)
import           Data.List                      (find, sortOn)
import qualified Data.Map                       as Map (elems, insert,
                                                        insertWith, lookup)
import           Data.Maybe                     (fromJust)
import           Data.Sequence                  (Seq, ViewL ((:<)), (><), (|>), (<|))
import qualified Data.Sequence                  as Seq (ViewL (EmptyL),
                                                        fromList, singleton,
                                                        viewl, zipWith, empty)
import           Prelude                        hiding (Either, lookup)
--------------------------------------------------------------------------------

string :: At Token -> Epilog ()
string (TokenStringLit s :@ p) =
    strings %= Map.insertWith (flip (><)) s (Seq.singleton p)
string _ = undefined


isSymbol' :: At String -> Epilog ()
isSymbol' (name :@ p) = do
    symbs <- use symbols
    unless (name `isSymbol` symbs) $
        err $ OutOfScope name p


checkBoth :: At Type -> At Type -> At Type
checkBoth (t1 :@ p1) (t2 :@ _t2) =
    ( if (t1 == None) || (t2 == None)
        then None
        else voidT
    ) :@ p1

checkDeclVar :: At Type -> At Name -> Epilog (At Type)
checkDeclVar (None :@ p) (_ :@ _) = return $ None :@ p
checkDeclVar (t :@ p) (var :@ _) = do
    symbs <- use symbols
    offs   <- use offset
    case var `local` symbs of
        Right Entry { eType, ePosition } -> do
            err $ DuplicateDeclaration var eType ePosition t p
            return $ None :@ p
        Left _ -> do
            symbols %= insertSymbol var (Entry var t Nothing p (head offs))
            offset  %= (\(x:xs) -> (x + padding (typeSize t)):xs)
            return $ voidT :@ p


checkInit :: At Type -> At Name -> At Type -> Epilog (At Type)
checkInit att atn (e :@ p) = do
    t :@ _ <- checkDeclVar att atn
    case t of
        Basic EpVoid _ ->
            if item att == e
                then return $ voidT :@ p
                else do
                    case e of
                        None -> return $ None :@ p
                        _ -> do
                            err $ InvalidAssign (item att) e p
                            return $ None :@ p

        _ -> return $ None :@ p

declStruct :: Epilog ()
declStruct = do
    Just (sName :@ p) <- use current
    ts                <- use types

    case sName `Map.lookup` ts of
        Just (_, p0) ->
            err $ DuplicateDefinition sName p0 p
        Nothing -> do
            Just struct' <- use curkind
            let  struct  =  toCons struct'
            fs <- use curfields
            forM_ fs (check sName)
            case struct' of
                (EitherK) -> do
                    let size = foldr (max.padding.typeSize.snd) 0 fs
                    types %= Map.insert sName (struct sName (toMap $offsE fs) size, p)
                (RecordK) -> do
                    let fs'  = offsR (Seq.viewl fs) 0
                    let size = foldr ((+).padding.typeSize.snd) 0 fs
                    types %= Map.insert sName (struct sName (toMap fs') size, p)

    current   .= Nothing
    curkind   .= Nothing
    curfields .= []

    where
        check sName (name :@ p, t) =
            case t of
                Array { inner } -> check sName (name :@ p, inner)
                Alias { name = n } ->
                    when (n == sName) $ err $ RecursiveType sName n p
                _ -> return ()
        offsR Seq.EmptyL _ = Seq.empty
        offsR ((name, t):<xs) offs =
            (name, (t,offs)) <| (offsR (Seq.viewl xs) (offs + (padding $ typeSize t)))
        offsE = fmap (\(n,t) -> (n,(t,0)))
        toMap                 = foldr toMap' []
        toMap' (name :@ _, t) = Map.insert name t

verifyField :: At Name -> Type -> Epilog ()
verifyField f@(n :@ p) t = do
    cf <- use curfields

    case find sameName cf of
        Just (_ :@ p1, t1) -> do
            Just (sName :@ sPos) <- use current
            Just k               <- use curkind
            err $ DuplicateField sName k sPos n t1 p1 t p
        Nothing ->
            curfields %= (|> (f, t))

    where
        sameName (n1 :@ _,_) = n == n1


getField :: At Type -> At Name -> Epilog (At Type)
getField (t :@ _) (fieldname :@ p) = case t of
    Alias tname _ -> do
        ts <- use types
        case tname `Map.lookup` ts of
            Just (Record _ fs _, _) -> checkField  fs
            Just (Either _ fs _, _) -> checkMember fs
            _                       -> err' InvalidAccess
    _  -> err' InvalidAccess

    where
        checkField fields = case fieldname `Map.lookup` fields of
            Just t'  -> return (fst t' :@ p)
            Nothing -> err' InvalidField
        checkMember members = case fieldname `Map.lookup` members of
            Just t'  -> return (fst t' :@ p)
            Nothing -> err' InvalidMember
        err' cons = do
            err $ cons fieldname (name t) p
            return (None :@ p)


findTypeOfSymbol :: At Name -> Epilog (At Type)
findTypeOfSymbol (name :@ p) = do
    symbs <- use symbols
    case name `lookup` symbs of
        Right (Entry _ t _ _ _) -> return (t :@ p)
        Left _ -> return (None :@ Position (0,0))


findType :: Name -> Epilog Type
findType tname = do
    ts <- use types
    ctype <- use current

    if not (null ctype) && tname == item (fromJust ctype)
        then return $ Alias tname 0
    else case tname `Map.lookup` ts of
        Just (t, _) -> case t of
            Record _ _ s -> return $ Alias tname s
            Either _ _ s -> return $ Alias tname s
            _            -> return t
        Nothing ->
            return $ Undef tname

checkArray :: At Type -> Type -> Epilog (At Type)
checkArray (Array _ _ t _ :@ p) index =
     if index == intT
        then return (t :@ p)
        else do
            err $ InvalidSubindex index p
            return (t :@ p)
checkArray (_ :@ p) _ = do
    err $ InvalidArray p
    return (None :@ p)

checkFor :: At Type -> At Type -> Epilog (At Type)
checkFor (t1 :@ rangep) (t2 :@ _) = do
    ((n :@ vp, t):_) <- use forVars
    if (t1 == t2 && t1 == t)
        then return $ voidT :@ rangep
        else do
            err $ InvalidRange n t vp t1 t2 rangep
            return $ None :@ rangep

deref :: At Type -> Epilog (At Type)
deref (Pointer t :@ p) =
    return $ t :@ p
deref (None :@ p) =
    return $ None :@ p
deref (t :@ p) = do
    err $ BadDeref t p
    return $ None :@ p

buildPointers :: Int -> Type -> Type
buildPointers 0 t = t
buildPointers n t = Pointer $ buildPointers (n-1) t

arrayPadding :: At Type -> At Type
arrayPadding (array@(Array _ _ _ _) :@ p) =
    array {sizeT = padding $ sizeT array} :@ p
arrayPadding t = t

buildArray :: Seq (Int32, Int32) -> Type  -> Type
buildArray _ x@(Undef _) = x
buildArray sizes t = case Seq.viewl sizes of
    Seq.EmptyL         -> t
    (low, high) :< lhs -> Array low high innerT size'

        where
            size'  = (typeSize  innerT) * (fromIntegral $ high - low + 1)
            innerT = (buildArray lhs t)


storeProcedure :: Type -> Epilog ()
storeProcedure t = do
    Just (n :@ p) <- use current
    (Scope { sEntries }, _) <- use symbols

    let params = Seq.fromList . map eType . sortOn ePosition . Map.elems $
            sEntries

    let entry' = Entry { eName         = n
                       , eType         = params :-> t
                       , eInitialValue = Nothing
                       , ePosition     = p
                       , eOffset       = 0
                       }

    symbols %= (\(Right st) -> st) . goUp
    symbols %= insertSymbol n (entry')
    symbols %= (\(Right st) -> st) . goDownLast

checkCall :: At Name -> Seq Type -> Epilog (At Type)
checkCall (pname :@ p) ts = do
    symbs <- use symbols
    case (pname `lookup` symbs) of
        Right (Entry _ (ets :-> ret) _ _ _) -> do
            if length ts == length ets && and (Seq.zipWith join ets ts)
                then return (ret :@ p)
                else do
                    err $ BadCall pname ts ets p
                    return (None :@ p)

        Right (Entry _ _ _ _ _) -> do
            err $ UndefinedProcedure pname p
            return (None :@ p)

        Left _ -> do
            err $ UndefinedProcedure pname p
            return (None :@ p)

    where
        join :: Type -> Type -> Bool
        join Any  _ = True
        join None _ = False
        join (OneOf ts') t  = t `elem` ts'
        join (Alias t1 _) (Alias t2 _) = t1 == t2
        join (Array {inner = i1}) (Array {inner = i2}) = join i1 i2
        join (Pointer p1) (Pointer p2) = join p1 p2
        join t1 t2 = t1 == t2


checkAssign :: At Type -> At Type -> Epilog (At Type)
checkAssign (lval :@ p) (e :@ _) = do
    if lval == e
        then return $ voidT :@ p
        else do
            case e of
                None -> return $ None :@ p
                _    -> do
                    err $ InvalidAssign lval e p
                    return $ None :@ p


checkAnswer :: Type -> Position -> Epilog (At Type)
checkAnswer None retp = return $ None :@ retp
checkAnswer aret retp = do
    Just proc <- use current
    (_ :-> eret) :@ procp <- findTypeOfSymbol proc

    if (eret == aret)
        then return $ voidT :@ retp
        else do
            if aret == voidT
                then err $ BadFinish eret      procp retp
                else err $ BadAnswer eret aret procp retp
            return $ None :@ retp


checkWrite :: Type -> Position -> Epilog (At Type)
checkWrite None p = return $ None :@ p
checkWrite t p =
    if t `elem` ([boolT, charT, intT, floatT, stringT] :: [Type])
        then return $ voidT :@ p
        else do
            err $ BadWrite t p
            return $ None :@ p


checkRead :: Type -> Position -> Epilog (At Type)
checkRead None p = return $ None :@ p
checkRead t p =
    if t `elem` ([boolT, charT, intT, floatT] :: [Type])
        then return $ voidT :@ p
        else do
            err $ BadRead t p
            return $ None :@ p


checkBinOp :: BinaryOp -> At Type -> At Type -> Epilog (At Type)
checkBinOp op = aux opTypes
    where
        aux _ (None :@ p) _        = return (None :@ p)
        aux _ (_ :@ p) (None :@ _) = return (None :@ p)
        aux [] (t1 :@ p) (t2 :@ _) = do
            err $ BadBinaryExpression op (t1, t2) (domain opTypes) p
            return (None :@ p)
        aux ((et1, et2, rt):ts) (t1 :@ p) (t2 :@ p') =
            if t1 == et1 && t2 == et2
                then return (rt :@ p)
                else aux ts (t1 :@ p) (t2 :@ p')
        opTypes = typeBinOp op
        domain = map (\(a, b, _) -> (a, b))

checkUnOp :: UnaryOp -> Position -> At Type -> Epilog (At Type)
checkUnOp op p = aux opTypes
    where
        aux _ (None :@ _) = return (None :@ p)
        aux [] (t1 :@ _) = do
            err $ BadUnaryExpression op t1 (domain opTypes) p
            return (None :@ p)
        aux ((et1, rt):ts) (t1 :@ p') =
            if t1 == et1
                then return (rt :@ p)
                else aux ts (t1 :@ p')
        opTypes = typeUnOp op
        domain = map fst


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
                     , ( floatT, floatT, floatT )
                     ]
typeBinOp Minus    = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     ]
typeBinOp Times    = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     ]

typeBinOp FloatDiv = [ ( floatT, floatT, floatT ) ]

typeBinOp IntDiv   = [ ( intT,   intT,   intT   ) ]
typeBinOp Rem      = [ ( intT,   intT,   intT   ) ]

typeBinOp LTop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     ]
typeBinOp LEop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     ]
typeBinOp GTop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     ]
typeBinOp GEop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     ]

typeBinOp EQop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     , ( boolT,  boolT,  boolT )
                     ]
typeBinOp NEop     = [ ( intT,   intT,   boolT )
                     , ( floatT, floatT, boolT )
                     , ( charT,  charT,  boolT )
                     , ( boolT,  boolT,  boolT )
                     ]

typeBinOp FAop     = [ ( intT,   intT,   boolT  ) ]
typeBinOp NFop     = [ ( intT,   intT,   boolT  ) ]


typeUnOp :: UnaryOp -> [(Type, Type)]
typeUnOp Not         = [ ( boolT,  boolT  ) ]
typeUnOp Bnot        = [ ( intT,   intT   ) ]
typeUnOp Uminus      = [ ( intT,   intT   )
                       , ( floatT, floatT )
                       ]
