{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Context
    ( isSymbol'
    , string
    , declVar
    , declStruct
    , findType
    , buildPointer
    , buildArray
    , storeProcedure
    , verifyField
    , boolOp
    , numOp
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                (use, (%=), (.=))
import           Control.Monad               (unless)
import           Data.Foldable               (toList)
import           Data.Int                    (Int32)
import           Data.List                   (sortOn, find)
import           Data.Maybe                  (fromJust)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map (elems, fromList, insert,
                                                     insertWith, lookup)
import           Data.Sequence               (Seq, (><))
import qualified Data.Sequence               as Seq (fromList, singleton)
import           Prelude                     hiding (Either)

import           Debug.Trace
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

declStruct :: At String -> [(At String, Type)]
           -> (String -> Map String Type -> Type)
           ->  Epilog ()
declStruct (sName :@ p) conts f = do
    ts <- use types
    case sName `Map.lookup` ts of
        Just (_,pos)  -> err $ DuplicateDefinition sName pos p
        Nothing -> do 
            l <- list (reverse conts)
            types %= Map.insert sName (f sName (Map.fromList l), p)
        where 
            list [] = return []
            list (x@(n :@ pos,t):xs) = if t == Alias sName
                    then err (RecursiveType sName n pos) >>
                         list xs >>= (\xs' -> return $ (n,t):xs')
                    else list xs >>= (\xs' -> return $ (n,t):xs')
            

verifyField :: (At String, Type) -> [(At String, Type)] 
            -> Epilog ([(At String, Type)])
verifyField x@(n :@ pos,t) l =
    case find (comp x) l of 
        Just (_ :@ p2, t2) -> do 
            err $ DuplicateDeclaration n t2 p2 t pos
            return l
        Nothing -> return (x:l)
    where comp (n1 :@ _,_) (n2 :@ _,_) = n1 == n2
        

findType :: At String -> Epilog (At Type)
findType (tname :@ p) = do
    ts <- use types
    ctype <- use current
    if not (null ctype) && tname == (snd $ fromJust ctype)
        then return (Alias tname :@ p)
    else case tname `Map.lookup` ts of
        Just (t, _) ->
            return $ t :@ p
        Nothing -> do
            err $ UndefinedType tname p
            return $ None :@ p


buildArray :: At Type -> Int32 -> Epilog (At Type)
buildArray (t :@ p) i =
    return $ (aux 0 (i-1) t) :@ p
    where
        aux _ _ None = None
        aux l h (Array l0 h0 i0) = Array l0 h0 (aux l h i0)
        aux l h o = Array l h o


buildPointer :: At Type -> Epilog (At Type)
buildPointer (t :@ p) = do
    return (Pointer t :@ p)


storeProcedure :: Type -> Epilog ()
storeProcedure t = do
    Just (p, n) <- use current
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
boolOp None _ = return None
boolOp _ None = return None
boolOp t1 t2 = if t1 == t2 && t1 == boolT
    then return t1
    else return None

numOp :: Type -> Type -> Epilog (Type)
numOp None _ = return None
numOp _ None = return None
numOp t1 t2 = if t1 == t2 && (t1 == intT || t1 == floatT)
    then return t1
    else return None
