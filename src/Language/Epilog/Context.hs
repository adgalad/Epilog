{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( buildArray
    , buildPointers
    , checkAnswer
    , checkArray
    , checkBinOp
    , checkCall
    , checkFor
    , checkUnOp
    , declStruct
    , declVar
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
import           Data.Sequence                  (Seq, ViewL ((:<)), (><), (|>))
import qualified Data.Sequence                  as Seq (ViewL (EmptyL),
                                                        fromList, singleton,
                                                        viewl, zipWith)
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


declVar :: At Type -> At String -> Epilog ()
declVar (None :@ _) (_ :@ _) = return ()
declVar (t :@ p) (var :@ _) = do
    symbs <- use symbols
    case var `local` symbs of
        Right Entry { eType, ePosition } ->
            err $ DuplicateDeclaration var eType ePosition t p
        Left _ ->
            symbols %= insertSymbol var (Entry var t Nothing p)

declStruct :: Epilog ()
declStruct = do
    Just (sName :@ p) <- use current
    ts                <- use types

    case sName `Map.lookup` ts of
        Just (_, p0) ->
            err $ DuplicateDefinition sName p0 p
        Nothing -> do
            Just struct' <- use curkind
            let struct = toCons struct'
            fs <- use curfields
            forM_ fs (check sName)
            types %= Map.insert sName (struct sName (toMap fs), p)

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
getField (t :@ _) (n :@ p) = case t of
    Record _ f -> checkField f
    Either _ f -> checkField f
    _          -> error'

    where
        checkField fields = case n `Map.lookup` fields of
            Just t'  -> return (t' :@ p)
            Nothing -> error'
        error' = do
            err $ InvalidMember n (name t) p
            return (None :@ p)



findTypeOfSymbol :: At Name -> Epilog (At Type)
findTypeOfSymbol (name :@ p) = do
    symbs <- use symbols
    case name `lookup` symbs of
        Right (Entry _ t _ _) -> return (t :@ p)
        Left _ -> return (None :@ Position (0,0))


findType :: Name -> Epilog Type
findType tname = do
    ts <- use types
    ctype <- use current

    if not (null ctype) && tname == item (fromJust ctype)
        then return $ Alias tname
    else case tname `Map.lookup` ts of
        Just (t, _) -> case t of
            Record _ _ -> return $ Alias tname
            Either _ _ -> return $ Alias tname
            _          -> return t
        Nothing ->
            return $ Undef tname

checkArray :: At Type -> Type -> Epilog (At Type)
checkArray (Array _ _ t :@ p) index =
     if index == intT
        then return (t :@ p)
        else do
            err $ InvalidSubindex (name index) p
            return (t :@ p)
checkArray (_ :@ p) _ = do
    err $ InvalidArray p
    return (None :@ p)

checkFor :: At Type -> At Type -> Epilog ()
checkFor (t1 :@ p) (t2 :@ _) =
    unless (t1 == t2 && (t1 == intT || t1 == charT)) $
        err $ InvalidRange (name t1) (name t2) p

buildPointers :: Int -> Type -> Type
buildPointers 0 t = t
buildPointers n t = Pointer $ buildPointers (n-1) t


buildArray :: Seq (Int32, Int32) -> Type  -> Type
buildArray _ x@(Undef _) = x
buildArray sizes t = case Seq.viewl sizes of
    Seq.EmptyL         -> t
    (low, high) :< lhs -> Array low high (buildArray lhs t)


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
                       }

    symbols %= (\(Right st) -> st) . goUp
    symbols %= insertSymbol n (entry')
    symbols %= (\(Right st) -> st) . goDownLast

checkCall :: At Name -> Seq Type -> Epilog (At Type)
checkCall (pname :@ p) ts = do
    symbs <- use symbols
    case (pname `lookup` symbs) of
        Right (Entry _ (ets :-> ret) _ _) -> do
            if length ts == length ets && and (Seq.zipWith join ets ts)
                then return (ret :@ p)
                else do
                    err $ BadCall pname ts ets p
                    return (None :@ p)

        Right (Entry _ _ _ _) -> do
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
        join (Alias t1) (Alias t2) = t1 == t2
        join (Array {inner = i1}) (Array {inner = i2}) = join i1 i2
        join (Pointer p1) (Pointer p2) = join p1 p2
        join t1 t2 = t1 == t2


checkAnswer :: Type -> Type -> Position -> Position -> Epilog ()
checkAnswer eret aret procp retp =
    unless (eret == aret) $
        if aret == voidT
            then err $ BadFinish eret      procp retp
            else err $ BadAnswer eret aret procp retp



checkBinOp :: BinaryOp -> Position -> Type -> Type -> Epilog (At Type)
checkBinOp op p = aux opTypes
    where
        aux [] t1 t2 = do
            err $ BadBinaryExpression op (t1, t2) (domain opTypes) p
            return (None :@ p)
        aux ((et1, et2, rt):ts) t1 t2 = if t1 == et1 && t2 == et2
            then return (rt :@ p)
            else aux ts t1 t2
        opTypes = typeBinOp op
        domain = map (\(a, b, _) -> (a, b))

checkUnOp :: UnaryOp -> Position -> Type -> Epilog (At Type)
checkUnOp op p = aux opTypes
    where
        aux [] t1 = do
            err $ BadUnaryExpression op t1 (domain opTypes) p
            return (None :@ p)
        aux ((et1, rt):ts) t1 = if t1 == et1
            then return (rt :@ p)
            else aux ts t1
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

typeBinOp LTop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     ]
typeBinOp LEop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     ]
typeBinOp GTop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     ]
typeBinOp GEop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     ]

typeBinOp EQop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     , ( boolT,  boolT,  boolT  )
                     ]
typeBinOp NEop     = [ ( intT,   intT,   intT   )
                     , ( floatT, floatT, floatT )
                     , ( charT,  charT,  charT  )
                     , ( boolT,  boolT,  boolT  )
                     ]

typeBinOp FAop     = [ ( intT,   intT,   boolT  ) ]
typeBinOp NFop     = [ ( intT,   intT,   boolT  ) ]


typeUnOp :: UnaryOp -> [(Type, Type)]
typeUnOp Not         = [ ( boolT,  boolT  ) ]
typeUnOp Bnot        = [ ( intT,   intT   ) ]
typeUnOp Uminus      = [ ( intT,   intT   )
                       , ( floatT, floatT )
                       ]
