{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Type
    ( Atom (..)
    , Type (..)
    , boolT
    , charT
    , intT
    , floatT
    , stringT
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
import           Language.Epilog.Common
--------------------------------------------------------------------------------
import           Data.List                (intercalate)
import           Data.Int                 (Int32)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Prelude                  hiding (Either)
--------------------------------------------------------------------------------
data Atom
    = EpBoolean
    | EpCharacter
    | EpInteger
    | EpFloat
    | EpString
    deriving (Eq)


instance Show Atom where
    show = \case
        EpBoolean   -> "boolean"
        EpCharacter -> "character"
        EpInteger   -> "integer"
        EpFloat     -> "float"
        EpString    -> "string"


instance Treelike Atom where
    toTree = leaf . show


data Type
    = Basic   { atom    :: Atom }
    | Pointer { pointed :: Type }
    | Array   { low     :: Int32, high :: Int32, item :: Type }
    | Record  { fields  :: Map Name Type }
    | Either  { fields  :: Map Name Type }
    | Any
    | None
    deriving (Eq)


instance Show Type where
    show = \case
        Basic   { atom }            -> show atom
        Pointer { pointed }         -> "pointer to " ++ show pointed
        Array   { low, high, item } ->
            "array [" ++ show low ++ "," ++ show high ++ ") of " ++ show item
        Record  { fields }          ->
            "record {" ++ show' fields ++ "}"
        Either  { fields }          ->
            "either {" ++ show' fields ++ "}"
        Any                         -> "any type"
        None                        -> "no type at all"

        where
            show' = intercalate ", " . Map.foldrWithKey aux []
            aux k a b = (k ++ " : " ++ show a) : b


instance Treelike Type where
    toTree = \case
        Basic   { atom }            -> leaf . show $ atom
        Pointer { pointed }         -> Node "pointer to" [toTree pointed]
        Array   { low, high, item } ->
            Node ("array [" ++ show low ++ "," ++ show high ++ ") of")
                [toTree item]
        Record  { fields }          -> Node "record" (toTree' fields)
        Either  { fields }          -> Node "either" (toTree' fields)
        Any                         -> leaf "any type"
        None                        -> leaf "no type at all"

        where
            toTree' = Map.foldrWithKey aux []
            aux k a b = Node k [toTree a] : b


boolT, charT, intT, floatT, stringT :: Type
boolT   = Basic EpBoolean
charT   = Basic EpCharacter
intT    = Basic EpInteger
floatT  = Basic EpFloat
stringT = Basic EpString
