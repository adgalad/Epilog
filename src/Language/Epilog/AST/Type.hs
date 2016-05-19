{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Type
    ( Type (..)
    , size
    , boolT
    , charT
    , floatT
    , intT
    , stringT
    , userT
    , voidT
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq (empty, null, length)
--------------------------------------------------------------------------------
data Type = Type
    { typeName  :: String
    , dimension :: Seq Int32
    } deriving (Eq)

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

size :: Type -> Int
size Type { dimension } = Seq.length dimension

boolT, charT, intT, floatT, stringT, voidT :: Type
boolT   = Type "boolean"   Seq.empty
charT   = Type "character" Seq.empty
intT    = Type "integer"   Seq.empty
floatT  = Type "float"     Seq.empty
stringT = Type "string"    Seq.empty
voidT   = Type "void"      Seq.empty

userT :: String -> Type
userT name = Type name Seq.empty
