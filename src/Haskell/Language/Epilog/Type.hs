{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Language.Epilog.Type
    ( Atom (..)
    , Type (..)
    , Mode (..)
    , Types
    , StructKind (..)
    , toCons
    , showS
    -- basic types
    , boolT
    , charT
    , intT
    , floatT
    , stringT
    , voidT
    , ptrT
    , scalar
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.Position (Position)
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import qualified Data.Foldable            as Foldable
import           Data.List                (intercalate)
import qualified Data.Map                 as Map
import           Prelude                  hiding (Either)
import           Data.Serialize         (Serialize)
import           GHC.Generics           (Generic)
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Types = Map Name (Type, Position)

----------------------------------------
data Atom
    = EpBoolean
    | EpCharacter
    | EpInteger
    | EpFloat
    | EpVoid
    deriving (Eq, Ord, Read, Generic, Serialize)


instance Show Atom where
    show = \case
        EpBoolean   -> "boolean"
        EpCharacter -> "character"
        EpInteger   -> "integer"
        EpFloat     -> "float"
        EpVoid      -> "void"


instance Treelike Atom where
    toTree = leaf . show

data Mode
    = RefMode
    | ValMode
    deriving (Eq, Ord, Read, Generic, Serialize)

instance Show Mode where
    show = \case
        RefMode -> "ref"
        ValMode -> "val"

data Type
    = Basic
        { atom   :: !Atom
        , sizeT  :: !Int
        , alignT :: !Int }
    | EpStr
        { sizeT  :: !Int
        , alignT :: !Int }
    | Array
        { low    :: Int32
        , high   :: Int32
        , inner  :: Type
        , sizeT  :: !Int
        , alignT :: !Int }
    | Record
        { name   :: String
        , fields :: Map Name (Type, Int)
        , sizeT  :: !Int
        , alignT :: !Int }
    | Either
        { name    :: String
        , members :: Map Name (Type, Int)
        , sizeT   :: !Int
        , alignT  :: !Int }
    | (:->)
        { params  :: Seq (Mode, Type)
        , returns :: Type }
    | Alias
        { name   :: !Name
        , sizeT  :: !Int
        , alignT :: !Int }
    | Pointer
        { pointed :: !Type
        , sizeT   :: !Int
        , alignT  :: !Int }
    | OneOf { options :: [Type] }
    | Any
    | None
    | Undef { name :: Name }
    deriving (Ord, Read, Generic, Serialize)

instance Eq Type where
    Basic { atom = a } == Basic { atom = b } =
        a == b
    EpStr _ _ == EpStr _ _ =
        True
    Array { inner = a } == Array { inner = b } =
        a == b
    Record { name = a } == Record { name = b } =
        a == b
    Either { name = a } == Either { name = b } =
        a == b
    Alias { name = a } == Alias { name = b } =
        a == b
    Pointer { pointed = a } == Pointer { pointed = b } =
        a == b
    OneOf { options = a } == OneOf { options = b } =
        a == b
    OneOf { options } == t =
        t `elem` options
    t == OneOf { options } =
        t `elem` options
    None == _ =
        False
    Any == _ =
        True

    (ps1 :-> r1) == (ps2 :-> r2) =
        r1 == r2 && ps1 == ps2

    _ == _ =
        False


instance Show Type where
    show = \case
        Basic   { atom }             -> show atom
        EpStr   {}                   -> "string"
        Pointer { pointed }          -> "pointer to " <> show pointed
        Array   { low, high, inner } ->
            "array [" <> show low <> "," <> show high <> "] of " <> show inner
        Record  { name, fields, sizeT } ->
            name <> " as record {" <> showFs fields <>
            "} (" <> showS sizeT <> ")"
        Either  { name, members, sizeT } ->
            name <> " as either {" <> showFs members <>
            "} (" <> showS sizeT <> ")"
        (:->)   { params, returns }  ->
            "procedure (" <> showPs params <> ") → " <> show returns
        Alias   { name }             -> name
        OneOf   { options }          -> "one of " <> show options
        Any                          -> "any type"
        None                         -> "no type at all"
        Undef   { name }             -> "undefined type `" <> name <> "`"

        where
            showFs = intercalate ", " . Map.foldrWithKey aux []
            aux k (t,_) b = (k <> " : " <> show t) : b
            showPs = intercalate " × " . Foldable.toList . fmap show


instance Treelike Type where
    toTree = \case
        Basic   { atom }             -> leaf . show $ atom
        EpStr   {}                   -> leaf "string"
        Pointer { pointed }          -> Node "pointer to" [ toTree pointed ]
        Array   { low, high, inner } ->
            Node ("array [" <> show low <> "," <> show high <> "] of")
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
            leaf $ "undefined type `" <> name <> "`"

        where
            toTreeFs = Map.foldrWithKey aux []
            aux k (t,offs) b = Node k [ toTree t
                                      , leaf ("Size: "   <> showS (sizeT t))
                                      , leaf ("Offset: " <> show offs)
                                      ] : b
            toTreePs = Foldable.toList . fmap toTreeP
            toTreeP (m, t) = Node (show m) [toTree t]


showS :: (Eq a, Num a, Show a) => a -> String
showS t = show t <> case t of
    1 -> " byte"
    _ -> " bytes"


boolT, charT, intT, floatT, stringT, voidT :: Type
boolT   = Basic   EpBoolean   0 0
charT   = Basic   EpCharacter 0 0
floatT  = Basic   EpFloat     0 0
intT    = Basic   EpInteger   0 0
stringT = EpStr               0 0
voidT   = Basic   EpVoid      0 0
ptrT    = Pointer Any         0 0 

scalar :: Type -> Bool
scalar Basic {}   = True
scalar Pointer {} = True
scalar _          = False

data StructKind = EitherK | RecordK deriving (Eq)


instance Show StructKind where
    show EitherK = "Either"
    show RecordK = "Record"


toCons :: StructKind -> Name -> Map Name (Type, Int) -> Int -> Int -> Type
toCons EitherK = Either
toCons RecordK = Record
