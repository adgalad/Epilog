{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Type
    ( Atom (..)
    , Type (..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq (null)
--------------------------------------------------------------------------------
data Atom
    = IntT | CharT | FloatT | BoolT | StringT | UserT String
    deriving (Eq)

instance Show Atom where
    show = \case
        BoolT       -> "boolean"
        CharT       -> "character"
        IntT        -> "integer"
        FloatT      -> "float"
        StringT     -> "string"
        UserT name  -> "user defined type `" ++ name ++ "`"

data Type = Type Atom (Seq Int32) deriving (Eq)

instance Show Type where
    show (Type atom dimensions) = if Seq.null dimensions
        then show atom
        else show atom ++ show (toList dimensions)

instance Treelike Type where
    toTree (Type atom dimensions) = if Seq.null dimensions
        then Node (show atom) []
        else Node
            ("array of " ++ show atom ++ " " ++ show (toList dimensions))
            []
