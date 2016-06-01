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
    , voidT
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Int                 (Int32)
import           Data.List                (intercalate)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Sequence            (Seq)
import qualified Data.Foldable            as Foldable
import           Prelude                  hiding (Either)
--------------------------------------------------------------------------------
data Atom
    = EpBoolean
    | EpCharacter
    | EpInteger
    | EpFloat
    | EpString
    | EpVoid
    deriving (Eq)


instance Show Atom where
    show = \case
        EpBoolean   -> "boolean"
        EpCharacter -> "character"
        EpInteger   -> "integer"
        EpFloat     -> "float"
        EpString    -> "string"
        EpVoid      -> "void"


instance Treelike Atom where
    toTree = leaf . show


data Type
    = Basic   { name    :: String,   atom    :: Atom }
    | Pointer { pointed :: Type }
    | Array   { low     :: Int32,    high    :: Int32, inner :: Type }
    | Record  { name    :: String,   fields  :: Map Name Name }
    | Either  { name    :: String,   fields  :: Map Name Name }
    | (:->)   { params  :: Seq Type, returns :: Type }
    | Any
    | None
    deriving (Eq)


instance Show Type where
    show = \case
        Basic   { name }             -> name
        Pointer { pointed }          -> "pointer to " ++ show pointed
        Array   { low, high, inner } ->
            "array [" ++ show low ++ "," ++ show high ++ ") of " ++ show inner
        Record  { name, fields }     ->
            name ++ " ~ record {" ++ showFs fields ++ "}"
        Either  { name, fields }     ->
            name ++ " ~ either {" ++ showFs fields ++ "}"
        (:->)   { params, returns }  ->
            "procedure (" ++ showPs params ++ ") → " ++ show returns
        Any                          -> "any type"
        None                         -> "no type at all"

        where
            showFs = intercalate ", " . Map.foldrWithKey aux []
            aux k a b = (k ++ " : " ++ show a) : b
            showPs = intercalate " × " . Foldable.toList . fmap show


instance Treelike Type where
    toTree = \case
        Basic   { name }             -> leaf name
        Pointer { pointed }          -> Node "pointer to" [toTree pointed]
        Array   { low, high, inner } ->
            Node ("array [" ++ show low ++ "," ++ show high ++ ") of")
                [toTree inner]
        Record  { name, fields }     ->
            Node (name ++ " ~ record") (toTreeFs fields)
        Either  { name, fields }     ->
            Node (name ++ " ~ either") (toTreeFs fields)
        (:->)   { params, returns }  ->
            Node "procedure"
                [ Node "parameters" (toTreePs params)
                , Node "returns" [toTree returns]
                ]
        Any                          -> leaf "any type"
        None                         -> leaf "no type at all"

        where
            toTreeFs = Map.foldrWithKey aux []
            aux k a b = Node k [leaf a] : b
            toTreePs = Foldable.toList . fmap toTree


boolT, charT, intT, floatT, stringT, voidT :: Type
boolT   = Basic "boolean"   EpBoolean
charT   = Basic "character" EpCharacter
intT    = Basic "integer"   EpInteger
floatT  = Basic "float"     EpFloat
stringT = Basic "string"    EpString
voidT   = Basic "void"      EpString
