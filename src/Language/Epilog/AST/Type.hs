{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Type
    ( Type (..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq (null)
--------------------------------------------------------------------------------

data Type = Type String (Seq Int32) deriving (Eq)

instance Show Type where
    show (Type t dimensions) = if Seq.null dimensions
        then "Type "++ t
        else "Type "++ t ++ show (toList dimensions)

instance Treelike Type where
    toTree (Type t dimensions) = if Seq.null dimensions
        then Node ("Type " ++ t) []
        else Node
            ("Array of " ++ t ++ " " ++ show (toList dimensions))
            []


