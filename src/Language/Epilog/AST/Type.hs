{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Type
    ( Type (..)
    , boolT
    , charT
    , intT
    , floatT
    , stringT
    , userT
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq (empty, null)
--------------------------------------------------------------------------------
data Type = Type String (Seq Int32) deriving (Eq)

instance Show Type where
    show (Type t dimensions) = "Type " ++ t ++ if Seq.null dimensions
        then ""
        else show (toList dimensions)

instance Treelike Type where
    toTree (Type t dimensions) = if Seq.null dimensions
        then Node ("Type " ++ t) []
        else Node
            ("Array of " ++ t ++ " " ++ show (toList dimensions))
            []

boolT, charT, intT, floatT, stringT :: Type
boolT   = Type "bool"   Seq.empty
charT   = Type "char"   Seq.empty
intT    = Type "int"    Seq.empty
floatT  = Type "float"  Seq.empty
stringT = Type "string" Seq.empty

userT :: String -> Type
userT name = Type name Seq.empty
