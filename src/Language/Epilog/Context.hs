{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( string
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




verifyDecl :: At String -> At String -> Epilog ()
verifyDecl (t :@ pos) (name :@ _) = do
    symbs <- use symbols
    ts    <- use types
    case t `Map.lookup` ts of
        Just (t0, posT) -> case name `local` symbs of
            Right Entry {eType, ePosition} ->
                err $ DuplicateDeclaration name eType ePosition t0 pos
            Left _ -> 
                    symbols %= insertSymbol name (Entry name t0 Nothing pos)
        Nothing ->
                err $ UndefinedType t name pos

            