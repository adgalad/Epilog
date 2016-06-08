{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( isSymbol'
    , string
    , declVar
    , declStruct
    , isFieldOf
    , findType
    , buildPointers
    , findTypeOfSymbol
    , checkArray
    , buildArray
    , storeProcedure
    , verifyField
    , boolOp
    , uNumOp
    , numOp
    , compOp
    , intOp
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Type
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
import           Language.Epilog.Common
--------------------------------------------------------------------------------
import           Control.Lens  (use, (%=), (.=))
import           Control.Monad (forM_, unless)
import           Data.Int      (Int32)
import           Data.List     (find, sortOn)
import qualified Data.Map      as Map (elems, insert, insertWith, lookup)
import           Data.Maybe    (fromJust)
import           Data.Sequence ((><), (|>))
import qualified Data.Sequence as Seq (fromList, singleton)
import           Prelude       hiding (Either, lookup)
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
                Alias { name = n } -> if n == sName
                    then err $ RecursiveType sName n p
                    else return ()
                _ -> return ()

        toMap                      = foldr toMap' []
        toMap' ((name :@ _), t) fs = Map.insert name t fs

verifyField :: (At String) -> Type -> Epilog ()
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

isFieldOf :: At String -> At Type -> Epilog (At Type)
isFieldOf (n :@ pos) (t :@ _) = case t of 
    Record _ f -> checkField f
    Either _ f -> checkField f
    _          -> error

    where checkField fields = case n `Map.lookup` fields of 
                Just t'  -> return (t' :@ pos)
                Nothing -> error
          error = do err $ InvalidMember n (name t) pos
                     return (None :@ pos)



findTypeOfSymbol :: At String -> Epilog (At Type)
findTypeOfSymbol (name :@ pos) = do 
    symbs <- use symbols
    case name `lookup` symbs of
        Right (Entry _ t _ _) -> return $ (t :@ pos)
        Left _ -> return (voidT :@ Position (0,0))

findType :: Name -> Epilog Type
findType tname = do
    ts <- use types
    ctype <- use current

    if not (null ctype) && tname == (item $ fromJust ctype)
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
            err $ InvalidIndex (name index) p
            return (t :@ p)
checkArray (_ :@ p) _ = do
    err $ InvalidArray p
    return (None :@ p)

buildPointers :: Int -> Type -> Epilog Type
buildPointers 0 t = return t
buildPointers n t = Pointer <$> buildPointers (n-1) t


buildArray :: Type -> (Int32, Int32) -> Epilog Type
buildArray x@(Undef _) _ =
    return x
buildArray t (low, high) =
    return $ Array low high t


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

    current .= Nothing

boolOp :: Type -> Type -> Epilog (Type)
boolOp t1 t2 = if t1 == t2 && t1 == boolT
    then return t1
    else return None

uNumOp :: Type -> Epilog (Type)
uNumOp t = if t == intT || t == floatT
    then return t
    else return None

numOp :: Type -> Type -> Epilog (Type)
numOp t1 t2 = if t1 == t2 && (t1 == intT || t1 == floatT)
    then return t1
    else return None

compOp :: Type -> Type -> Epilog (Type)
compOp t1 t2 = 
    if t1==t2 && (t1 == intT || t1 == floatT)
        then return boolT
        else return None

intOp :: Type -> Type -> Epilog (Type)
intOp t1 t2 = if t1 == t2 && t1 == intT
    then return t1
    else return None
