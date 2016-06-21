{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Type
    ( Atom (..)
    , Type (..)
    , StructKind (..)
    , toCons
    , showS
    , padding
    -- basic types
    , boolT
    , charT
    , intT
    , floatT
    , stringT
    , typeSize
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
    = Basic
        { atom  :: Atom
        , sizeT :: Int
        }
    | Array
        { low   :: Int32
        , high  :: Int32
        , inner :: Type
        , sizeT :: Int
        }
    | Record
        { name   :: String
        , fields :: Map Name (Type, Int)
        , sizeT  :: Int
        }
    | Either
        { name    :: String
        , members :: Map Name (Type, Int)
        , sizeT   :: Int
        }
    | (:->)
        { params  :: Seq Type
        , returns :: Type
        }
    | Alias
        { name  :: Name
        , sizeT :: Int
        }
    | Pointer { pointed :: Type }
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
        Record  { name, fields, sizeT } ->
            name ++ " as record {" ++ showFs fields ++
            "} (" ++ showS sizeT ++ ")"
        Either  { name, members, sizeT } ->
            name ++ " as either {" ++ showFs members ++
            "} (" ++ showS sizeT ++ ")"
        (:->)   { params, returns }  ->
            "procedure (" ++ showPs params ++ ") → " ++ show returns
        Alias   { name }             -> name
        OneOf   { options }          -> "one of " ++ show options
        Any                          -> "any type"
        None                         -> "no type at all"
        Undef   { name }             -> "undefined type `" ++ name ++ "`"

        where
            showFs = intercalate ", " . Map.foldrWithKey aux []
            aux k (t,_) b = (k ++ " : " ++ show t) : b
            showPs = intercalate " × " . Foldable.toList . fmap show



instance Treelike Type where
    toTree = \case
        Basic   { atom }             -> leaf . show $ atom
        Pointer { pointed }          -> Node "pointer to" [ toTree pointed ]
        Array   { low, high, inner } ->
            Node ("array [" ++ show low ++ "," ++ show high ++ "] of")
                [ toTree inner ]
        Record  { fields, sizeT }     ->
            Node "record"
                [ Node "size" [leaf . showS $ sizeT]
                , Node "fields" (toTreeFs fields)
                ]
        Either  { members, sizeT }     ->
            Node "either"
                [ Node "size" [leaf . showS $ sizeT]
                , Node "members" (toTreeFs members)
                ]
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
            aux k (t,offs) b = Node k [toTree t
                                      , leaf ("Size: "   ++ showS (typeSize t))
                                      , leaf ("Offset: " ++ show offs)
                                      ] : b
            toTreePs = Foldable.toList . fmap toTree

typeSize :: Type -> Int
typeSize t = case t of
    Basic     _ s -> s
    Array _ _ _ s -> s
    Record  _ _ s -> s
    Either  _ _ s -> s
    Alias     _ s -> s
    Pointer     _ -> 4
    _             -> undefined

showS :: (Eq a, Num a, Show a) => a -> String
showS t = show t ++ case t of
    1 -> " byte"
    _ -> " bytes"

padding :: Int -> Int
padding size = size + ((4 - size `mod` 4) `mod` 4)


boolT, charT, intT, floatT, stringT, voidT :: Type
boolT   = Basic EpBoolean   1
charT   = Basic EpCharacter 1
intT    = Basic EpInteger   4
floatT  = Basic EpFloat     4
stringT = Basic EpString    4
voidT   = Basic EpVoid      0

data StructKind = EitherK | RecordK
                deriving (Eq)

instance Show StructKind where
    show EitherK = "Either"
    show RecordK = "Record"

toCons :: StructKind -> Name -> Map Name (Type, Int) -> Int -> Type
toCons EitherK = Either
toCons RecordK = Record
