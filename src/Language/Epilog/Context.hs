{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( isSymbol'
    , string
    , verifyDecl
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Lexer
import           Language.Epilog.Error
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens           ((%=), use)
import           Data.Sequence          ((><))
import qualified Data.Sequence          as Seq (singleton)
import qualified Data.Map               as Map (insertWith, lookup)
--------------------------------------------------------------------------------

string :: At Token -> Epilog ()
string (TokenStringLit s :@ p) = do
    strings %= Map.insertWith (flip (><)) s (Seq.singleton p)


--either :: Position -> String -> Class -> Conts
--either pos name conts = do
--    symbs <- use symbols
--    t <- use types
--    case name `Map.lookup` t of
--        Just (_, _, p) -> err $ DuplicateDefinition name p pos
--        Nothing -> do
--            types %= Map.insert name (Either conts)

isSymbol' :: At String -> Epilog ()
isSymbol' (name :@ pos) = do
    symbs <- use symbols
    if name `isSymbol` symbs 
        then return ()
        else err $ OutOfScope name pos


verifyDecl :: At String -> At String -> Epilog ()
verifyDecl (t :@ p) (name :@ _) = do
    symbs <- use symbols
    ts    <- use types
    case t `Map.lookup` ts of
        Just (t0, _posT) -> case name `local` symbs of
            Right Entry {eType, ePosition} ->
                err $ DuplicateDeclaration name eType ePosition t0 p
            Left _ ->
                    symbols %= insertSymbol name (Entry name t0 Nothing p)
        Nothing ->
                err $ UndefinedType t name p

