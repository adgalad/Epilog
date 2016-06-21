{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( buildArray
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
    , prepare
    , storeProcedure
    , storeProcedure'
    , string
    , verifyField
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import qualified Language.Epilog.AST.AST        as AST
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
import           Control.Monad.Trans.RWS.Strict (asks)
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


isSymbol' :: At String -> Epilog ()
isSymbol' (name :@ p) = do
    symbs <- use symbols
    unless (name `isSymbol` symbs) $
        err $ OutOfScope name p


checkBoth :: At Type -> At Type -> At Type
checkBoth (t1 :@ p1) (t2 :@ _t2) =
    ( if (t1 == None) || (t2 == None)
        then None
        else EpVoid
    ) :@ p1

checkDeclVar :: At Type -> At Name -> Epilog (At Type)
checkDeclVar (None :@ p) (_ :@ _) = return $ None :@ p
checkDeclVar (t :@ p) (var :@ _) = do
    symbs <- use symbols
    offs  <- use offset
    case var `local` symbs of
        Right Entry { eType, ePosition } -> do
            err $ DuplicateDeclaration var eType ePosition t p
            return $ None :@ p
        Left _ -> do
            symbols %= insertSymbol var
                (entry var t p (padded (typeAlign t) (head offs)))
            offset  %= \(x:xs) -> (padded (typeAlign t) x + typeSize t : xs)
            return $ EpVoid :@ p


checkInit :: At Type -> At Name -> At Type -> Epilog (At Type)
checkInit att atn (e :@ p) = do
    t :@ _ <- checkDeclVar att atn
    case t of
        EpVoid ->
            if item att == OneOf [boolT, charT, intT, floatT, Pointer Any 0 0]
                then if item att == e
                    then return $ EpVoid :@ p
                    else case e of
                        None -> return $ None :@ p
                        _ -> do
                            err $ AssignMismatch (item att) e p
                            return $ None :@ p
                else do
                    err $ NonBasicAssign (item att) p
                    return $ None :@ p
        _ -> return $ None :@ p


pad :: Int -> Int -> Int
pad a o = (a - (o `mod` a)) `mod` a


padded :: Int -> Int -> Int
padded a o = o + pad a o


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
                    let newOffset = padded (typeAlign t) o + typeSize t
                    offset       .= newOffset : os
                    structAlign  %= max (typeAlign t)
                    structSize   .= newOffset
                EitherK -> do
                    structAlign %= max (typeAlign t)
                    structSize  %= max (typeSize t)
    where
        sameName (n1 :@ _,_,_) = n == n1


getField :: At Type -> At Name -> Epilog (At Type)
getField (t :@ _) (fieldname :@ p) = case t of
    Alias tname _ _ -> do
        ts <- use types
        case tname `Map.lookup` ts of
            Just (Record _ fs _ _, _) -> checkField  fs
            Just (Either _ ms _ _, _) -> checkMember ms
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
        then return $ Alias tname 0 0
    else case tname `Map.lookup` ts of
        Just (t, _) -> case t of
            Record _ _ s a -> return $ Alias tname s a
            Either _ _ s a -> return $ Alias tname s a
            _              -> return t
        Nothing ->
            return $ Undef tname

checkArray :: At Type -> Type -> Epilog (At Type)
checkArray (Array _ _ t _ _ :@ p) index =
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
    if t1 == t2 && t1 == t
        then return $ EpVoid :@ rangep
        else do
            err $ InvalidRange n t vp t1 t2 rangep
            return $ None :@ rangep

deref :: At Type -> Epilog (At Type)
deref (Pointer t _ _ :@ p) =
    return $ t :@ p
deref (None :@ p) =
    return $ None :@ p
deref (t :@ p) = do
    err $ BadDeref t p
    return $ None :@ p

buildPointers :: Int -> Type -> Epilog Type
buildPointers 0 t = return t
buildPointers n t = do
    psize <- asks pointerSize
    palg  <- asks pointerAlign
    inner <- buildPointers (n-1) t
    return $ Pointer inner psize palg


buildArray :: Seq (Int32, Int32) -> Type  -> Type
buildArray _ x@(Undef _) = x
buildArray sizes t = case Seq.viewl sizes of
    Seq.EmptyL         -> t
    (low, high) :< lhs -> Array low high innerT size' al'

        where
            innerT = buildArray lhs t
            al'    = typeAlign innerT
            size'  = typeSize  innerT * fromIntegral (high - low + 1)


storeProcedure' :: At Type -> Epilog ()
storeProcedure' (instType :@ _)= do
    Just (n :@ p) <- use current
    t <- use curProcType
    symbols %= (\(Right st) -> st) . goUp
    if instType == voidT
        then do
            procInsts <- AST.topInsts
            let entry' = Entry { eName         = n
                               , eType         = t
                               , eInitialValue = Nothing
                               , eAST          = Just procInsts
                               , ePosition     = p
                               , eOffset       = 0
                               }
            AST.insert $ ProcDecl p t n procInsts
            current .= Nothing
            symbols %= insertSymbol n entry'
        else do
            let entry' = Entry { eName         = n
                               , eType         = t
                               , eInitialValue = Nothing
                               , eAST          = Nothing
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


checkCall :: At Name -> Seq Type -> Epilog (At Type)
checkCall (pname :@ _) ts = do
    symbs <- use symbols
    Just (n :@ p) <- use current
    (ets :-> ret) <- use curProcType
    if pname == n then
        if length ts == length ets && and (Seq.zipWith (==) ets ts)
                then return (ret :@ p)
                else do
                    err $ BadCall pname ts ets p
                    return (None :@ p)
        else case pname `lookup` symbs of
            Right (Entry _ (ets :-> ret) _ _ _) ->
                if length ts == length ets && and (Seq.zipWith (==) ets ts)
                    then return (ret :@ p)
                    else do
                        err $ BadCall pname ts ets' p
                        return (None :@ p)

            Right _ -> do
                err $ UndefinedProcedure pname p
                return (None :@ p)

            Left _ -> do
                err $ UndefinedProcedure pname p
                return (None :@ p)


checkAssign :: At Type -> At Type -> Epilog (At Type)
checkAssign (lval :@ p) (e :@ _) = if lval == e
    then return $ EpVoid :@ p
    else case e of
        None -> return $ None :@ p
        _    -> do
            err $ AssignMismatch lval e p
            return $ None :@ p


checkAnswer :: Type -> Position -> Epilog (At Type)
checkAnswer None retp = return $ None :@ retp
checkAnswer aret retp = do
    Just proc <- use current
    (_ :-> eret) :@ procp <- findTypeOfSymbol proc

    if eret == aret
        then return $ EpVoid :@ retp
        else do
            if aret == EpVoid
                then err $ BadFinish eret      procp retp
                else err $ BadAnswer eret aret procp retp
            return $ None :@ retp


checkWrite :: Type -> Position -> Epilog (At Type)
checkWrite None p = return $ None :@ p
checkWrite t p =
    if t `elem` ([boolT, charT, intT, floatT, stringT] :: [Type])
        then return $ EpVoid :@ p
        else do
            err $ BadWrite t p
            return $ None :@ p


checkRead :: Type -> Position -> Epilog (At Type)
checkRead None p = return $ None :@ p
checkRead t p =
    if t `elem` ([boolT, charT, intT, floatT] :: [Type])
        then return $ EpVoid :@ p
        else do
            err $ BadRead t p
            return $ None :@ p


checkBinOp :: BinaryOp -> Position
           -> At Type  -> At Type
           -> Epilog (At Type)
checkBinOp op p = aux opTypes
    where
        aux _ (None :@ _) _  = return (None :@ p)
        aux _ _ (None :@ _)  = return (None :@ p)
        aux [] (t1 :@ _) (t2 :@ _) = do
            err $ BadBinaryExpression op (t1, t2) (domain opTypes) p
            return (None :@ p)
        aux ((et1, et2, rt):ts) (t1 :@ p) (t2 :@ p') =
            if t1 == et1 && t2 == et2
                then return (rt :@ p)
                else aux ts (t1 :@ p) (t2 :@ p')
        opTypes = typeBinOp op
        domain = map (\(a, b, _) -> (a, b))

checkUnOp :: UnaryOp -> Position
          -> At Type -> Epilog (At Type)
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
