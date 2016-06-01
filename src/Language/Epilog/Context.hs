{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( isSymbol'
    , string
    , declVar
    , declStruct
    , findType
    , buildPointer
    , buildArray
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Lexer
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                (use, (%=))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map (insertWith, lookup, fromList, insert)
import           Data.Sequence               (Seq,(><))
import qualified Data.Sequence               as Seq (singleton)
import           Data.Int                    (Int32)
import           Data.Foldable               (toList)
--------------------------------------------------------------------------------

string :: At Token -> Epilog ()
string (TokenStringLit s :@ p) = do
    strings %= Map.insertWith (flip (><)) s (Seq.singleton p)
string _ = undefined


isSymbol' :: At String -> Epilog ()
isSymbol' (name :@ p) = do
    symbs <- use symbols
    if name `isSymbol` symbs
        then return ()
        else err $ OutOfScope name p




declVar :: At Type -> At String -> Epilog ()
declVar (None :@ _) (_ :@ _) = return ()
declVar (t :@ p) (var :@ _) = do
    symbs <- use symbols
    case var `local` symbs of
        Right Entry { eType, ePosition } ->
            err $ DuplicateDeclaration var eType ePosition t p
        Left _ ->
            symbols %= insertSymbol var (Entry var t Nothing p)

declStruct :: At String -> Seq (String, Type) -> (String-> Map String Type ->Type) ->  Epilog ()
declStruct (name :@ pos) conts f = do
    ts <- use types
    case name `Map.lookup` ts of
        Just (t, _) -> return ()
        Nothing -> 
            types %= Map.insert name (f name (Map.fromList $ toList conts), pos)
            

findType :: At String -> Epilog (At Type)
findType (tname :@ p) = do
    ts <- use types
    case tname `Map.lookup` ts of
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
    return (t :@ p)
