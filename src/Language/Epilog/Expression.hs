{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.Expression
    ( Expression(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexeme

import           Data.Int               (Int32)
--------------------------------------------------------------------------------

data Expression
    = LitInt (Lexeme Int32)
    deriving (Eq)

instance Show Expression where
    show = \case
        LitInt (Lexeme _ value) -> "LitInt " ++ show value