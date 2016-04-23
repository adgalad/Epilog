{-# LANGUAGE LambdaCase    #-}

module Language.Epilog.Error
    ( Error(..)
    , isError
    , LexerError(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Token
import           Language.Epilog.Position

import           Data.Function         (on)
--------------------------------------------------------------------------------

data Error
    = LError Position LexerError

instance Show Error where
    show = \case
        LError p e -> "Lexer error " ++ show p ++ ":\n\t" ++ show e ++ "\n"

instance Eq Error where
  (==) = (==) `on` errorPos

instance Ord Error where
    compare = compare `on` errorPos

isError :: Error -> Bool
isError _ = True

data LexerError
    = LexerError     String
    | UnexpectedChar Char
    | StringError    String
    -- Support
    | TokenNotSupported Token

instance Show LexerError where
    show = \case
        LexerError msg       ->
            msg
        UnexpectedChar c     ->
            "unexpected character '" ++ [c] ++ "'"
        StringError str      ->
            "missing matching quotation mark for string " ++ show str
        TokenNotSupported tk ->
            show tk ++ " is not supported yet"

errorPos :: Error -> Position
errorPos = \case
    LError p _ -> p
