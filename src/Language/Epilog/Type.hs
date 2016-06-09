{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Type
    ( Atom (..)
    , Type (..)
    , StructKind (..)
    , toCons
    -- basic types
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
    = Basic   { atom    :: Atom }
    | Pointer { pointed :: Type }
    | Array   { low     :: Int32,    high    :: Int32, inner :: Type }
    | Record  { name    :: String,   fields  :: Map Name Type }
    | Either  { name    :: String,   fields  :: Map Name Type }
    | (:->)   { params  :: Seq Type, returns :: Type }
    | Alias   { name    :: Name }
    | OneOf   { options :: [Type] }
    | Any
    | None
    | Undef   { name :: Name}
    deriving (Eq)


instance Show Type where
    show = \case
        Basic   { atom }             -> show atom
        Pointer { pointed }          -> "pointer to " ++ show pointed
        Array   { low, high, inner } ->
            "array [" ++ show low ++ "," ++ show high ++ "] of " ++ show inner
        Record  { name, fields }     ->
            name ++ " ~ record {" ++ showFs fields ++ "}"
        Either  { name, fields }     ->
            name ++ " ~ either {" ++ showFs fields ++ "}"
        (:->)   { params, returns }  ->
            "procedure (" ++ showPs params ++ ") → " ++ show returns
        Alias   { name }             -> name
        OneOf   { options }          -> "one of " ++ show options
        Any                          -> "any type"
        None                         -> "no type at all"
        Undef   { name }             -> "undefined type `" ++ name ++ "`"

        where
            showFs = intercalate ", " . Map.foldrWithKey aux []
            aux k a b = (k ++ " : " ++ show a) : b
            showPs = intercalate " × " . Foldable.toList . fmap show


instance Treelike Type where
    toTree = \case
        Basic   { atom }             -> leaf (show atom)
        Pointer { pointed }          -> Node "pointer to" [ toTree pointed ]
        Array   { low, high, inner } ->
            Node ("array [" ++ show low ++ "," ++ show high ++ "] of")
                [ toTree inner ]
        Record  { name, fields }     ->
            Node (name ++ " ~ record") (toTreeFs fields)
        Either  { name, fields }     ->
            Node (name ++ " ~ either") (toTreeFs fields)
        (:->)   { params, returns }  ->
            Node "procedure"
                [ Node "parameters" (toTreePs params)
                , Node "returns" [toTree returns]
                ]
        Alias   { name }             -> leaf name
        OneOf   { options }          -> Node "One of" (toForest options)
        Any                          -> leaf "any type"
        None                         -> leaf "no type at all"
        Undef   { name }             ->
            leaf $ "undefined type `" ++ name ++ "`"

        where
            toTreeFs = Map.foldrWithKey aux []
            aux k a b = Node k [toTree a] : b
            toTreePs = Foldable.toList . fmap toTree


boolT, charT, intT, floatT, stringT, voidT :: Type
boolT   = Basic EpBoolean
charT   = Basic EpCharacter
intT    = Basic EpInteger
floatT  = Basic EpFloat
stringT = Basic EpString
voidT   = Basic EpVoid

data StructKind = EitherK | RecordK
                deriving (Eq)

instance Show StructKind where
    show EitherK = "Either"
    show RecordK = "Record"

toCons :: StructKind -> Name -> Map Name Type -> Type
toCons EitherK = Either
toCons RecordK = Record
