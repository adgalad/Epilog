{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Language.Epilog.SymbolTable
    ( Entry (..)
    , SymbolTable (..)
    , Zipper
    , focus
    , defocus
    , insertSymbol
    , emptyST
    , insertST
    , openScope
    , closeScope
    , local
    , lookup
    , root
    , goDownFirst
    , goDownLast
    , goBack
    , goLeft
    , goRight
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map hiding (Map)
import           Data.Maybe                     (fromJust)
import           Data.Sequence                  (Seq, ViewL ((:<)),
                                                 ViewR ((:>)), (<|), (><), (|>))
import qualified Data.Sequence                  as Seq
import           Prelude                        hiding (lookup)
--------------------------------------------------------------------------------
-- Symbol Table Entry ------------------
data Entry = Entry
    { varName         :: String
    , varType         :: Type
    , varInitialValue :: Maybe Expression
    , varPosition     :: (Int, Int)
    }

instance Treelike Entry where
    toTree Entry {varName, varType, varInitialValue, varPosition} =
        Node ("Variable `" ++ varName ++ "`") $
            Node ("Declared at " ++ show varPosition) [] :
            Node ("Type: " ++ show varType) [] :
            case varInitialValue of
                Nothing -> [Node "Not initialized" []]
                Just e -> [Node "Initialized with value" [toTree e]]

-- Symbol Table Scope ------------------
data Scope = Scope
    { sFrom    :: (Int, Int)
    , sTo      :: (Int, Int)
    , sEntries :: Map String Entry
    }

instance Treelike Scope where
    toTree Scope {sEntries} =
        if Map.null sEntries
            then Node "No symbols" []
            else Node "Symbols" (toForest . Map.elems $ sEntries)

lookup'' :: String -> Scope -> Maybe Entry
lookup'' key Scope {sEntries} =
    Map.lookup key sEntries

insert'' :: String -> Entry -> Scope -> Scope
insert'' key entry s@Scope {sEntries} =
    s {sEntries = Map.insert key entry sEntries}

emptyScope :: (Int, Int) -> Scope
emptyScope position = Scope
    { sFrom = position
    , sTo   = position
    , sEntries = Map.empty
    }

-- Symbol Table ------------------------
data SymbolTable = SymbolTable
    { stScope    :: Scope
    , stChildren :: SymbolTables
    }

type SymbolTables = Seq SymbolTable

instance Treelike SymbolTable where
    toTree SymbolTable { stScope = scope @ Scope{sFrom, sTo}, stChildren } =
        Node ("Scope " ++ show sFrom ++ " -> " ++ show sTo) $
            toTree scope :
            toForest stChildren

lookup' :: String -> SymbolTable -> Maybe Entry
lookup' key SymbolTable { stScope } =
    lookup'' key stScope

insert' :: String -> Entry -> SymbolTable -> SymbolTable
insert' key entry st @ SymbolTable { stScope } =
    st { stScope = insert'' key entry stScope }

insertST' :: SymbolTable -> SymbolTable -> SymbolTable
insertST' newST st @ SymbolTable { stChildren } =
    st { stChildren = stChildren |> newST }

exit' :: (Int, Int) -> SymbolTable -> SymbolTable
exit' position st @ SymbolTable { stScope } =
    st { stScope = stScope { sTo = position } }

emptyST :: (Int, Int) -> SymbolTable
emptyST position = SymbolTable
    { stScope    = emptyScope position
    , stChildren = Seq.empty
    }

-- Symbol Table Zipper -----------------
data Breadcrumb = Breadcrumb
    { parent :: Scope
    , left   :: SymbolTables
    , right  :: SymbolTables
    }

type Zipper = (SymbolTable, [Breadcrumb])

---- Moving around -----------
goDownFirst :: Zipper -> Maybe Zipper
goDownFirst (SymbolTable {stScope, stChildren}, bs)
    | Seq.null stChildren = Nothing
    | otherwise = Just (x, Breadcrumb stScope Seq.empty xs : bs)
    where
        x :< xs = Seq.viewl stChildren

goDownLast :: Zipper -> Maybe Zipper
goDownLast (SymbolTable {stScope, stChildren}, bs)
    | Seq.null stChildren = Nothing
    | otherwise = Just (x, Breadcrumb stScope xs Seq.empty : bs)
    where
        xs :> x = Seq.viewr stChildren

goRight :: Zipper -> Maybe Zipper
goRight (_, []) =
    Nothing
goRight (st, Breadcrumb {parent, left, right} : bs)
    | Seq.null right = Nothing
    | otherwise = Just (r, Breadcrumb parent (left |> st) right' : bs)
    where
        r :< right' = Seq.viewl right

goLeft :: Zipper -> Maybe Zipper
goLeft (_, []) =
    Nothing
goLeft (st, Breadcrumb {parent, left, right} : bs)
    | Seq.null right = Nothing
    | otherwise = Just (l, Breadcrumb parent left' (st <| right) : bs)
    where
        left' :> l = Seq.viewr left

goBack :: Zipper -> Maybe Zipper
goBack (_, []) = Nothing
goBack (st, Breadcrumb {parent, left, right} : bs) =
    Just (SymbolTable parent ((left |> st) >< right), bs)

root :: Zipper -> Zipper
root (st, []) = (st, [])
root z = root . fromJust . goBack $ z

---- (de)focusing ------------
focus :: SymbolTable -> Zipper
focus = (,[])

defocus :: Zipper -> SymbolTable
defocus = fst

---- Using the table ---------
lookup :: String -> Zipper -> Maybe Entry
lookup key (st, []) =
    lookup' key st
lookup key zipper@(st, _) =
    case lookup' key st of
        Nothing -> lookup key . fromJust . goBack $ zipper
        justEntry -> justEntry

local :: String -> Zipper -> Maybe Entry
local key (st, _) =
    lookup' key st

insertSymbol :: String -> Entry -> Zipper -> Zipper
insertSymbol key entry (st, bs) =
    (insert' key entry st, bs)

insertST :: SymbolTable -> Zipper -> Zipper
insertST newST (st, bs) =
    (insertST' newST st, bs)

openScope :: (Int, Int) -> Zipper -> Zipper
openScope pos =
    fromJust . goDownLast . insertST (emptyST pos)

closeScope :: (Int, Int) -> Zipper -> Zipper
closeScope pos (st, bs) =
    (exit' pos st, bs)
