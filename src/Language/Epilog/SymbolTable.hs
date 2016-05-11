{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Language.Epilog.SymbolTable
    ( Entry (..)
    , SymbolTable (..)
    , Zipper
    , closeScope
    , defocus
    , empty
    , focus
    , goDownFirst
    , goDownLast
    , goNext
    , goPrevious
    , goUp
    , insertST
    , insertSymbol
    , local
    , lookup
    , openScope
    , root
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map hiding (Map)
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
    toTree Entry { varName, varType, varInitialValue, varPosition } =
        Node ("Variable `" ++ varName ++ "`") $
            Node ("Declared at " ++ show varPosition) [] :
            Node ("Type: " ++ show varType) [] :
            case varInitialValue of
                Nothing -> [Node "Not initialized" []]
                Just e  -> [Node "Initialized with value" [toTree e]]

-- Symbol Table Scope ------------------
data Scope = Scope
    { sFrom    :: (Int, Int)
    , sTo      :: (Int, Int)
    , sEntries :: Map String Entry
    }

instance Treelike Scope where
    toTree Scope { sEntries } =
        if Map.null sEntries
            then Node "No symbols" []
            else Node "Symbols" (toForest . Map.elems $ sEntries)

lookup'' :: String -> Scope -> Either String Entry
lookup'' key Scope { sEntries } =
    case Map.lookup key sEntries of
        Just entry -> Right entry
        Nothing    -> Left "Not found."

insert'' :: String -> Entry -> Scope -> Scope
insert'' key entry s@Scope { sEntries } =
    s { sEntries = Map.insert key entry sEntries }

empty'' :: (Int, Int) -> Scope
empty'' position = Scope
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
    toTree SymbolTable { stScope = scope@Scope { sFrom, sTo }, stChildren } =
        Node ("Scope " ++ show sFrom ++ " -> " ++ show sTo) $
            toTree scope :
            toForest stChildren

lookup' :: String -> SymbolTable -> Either String Entry
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

empty' :: (Int, Int) -> SymbolTable
empty' position = SymbolTable
    { stScope    = empty'' position
    , stChildren = Seq.empty
    }

-- Symbol Table Zipper -----------------
data Breadcrumb = Breadcrumb
    { parent :: Scope
    , left   :: SymbolTables
    , right  :: SymbolTables
    }

type Zipper = (SymbolTable, [Breadcrumb])

---- Starter Zipper ----------
empty :: Zipper
empty = focus $ empty' (0, 0)

---- Moving around -----------
goDownFirst :: Zipper -> Either String Zipper
goDownFirst (SymbolTable { stScope, stChildren }, bs)
    | Seq.null stChildren = Left "No embedded scopes."
    | otherwise           = Right (x, Breadcrumb stScope Seq.empty xs : bs)
    where
        x :< xs = Seq.viewl stChildren

goDownLast :: Zipper -> Either String Zipper
goDownLast (SymbolTable { stScope, stChildren }, bs)
    | Seq.null stChildren = Left "No embedded scopes."
    | otherwise           = Right (x, Breadcrumb stScope xs Seq.empty : bs)
    where
        xs :> x = Seq.viewr stChildren

goNext :: Zipper -> Either String Zipper
goNext (_, []) =
    Left "Root scope has no siblings."
goNext (st, Breadcrumb { parent, left, right } : bs)
    | Seq.null right = Left "Already at last scope."
    | otherwise      = Right (r, Breadcrumb parent (left |> st) right' : bs)
    where
        r :< right' = Seq.viewl right

goPrevious :: Zipper -> Either String Zipper
goPrevious (_, []) =
    Left "Root scope has no siblings."
goPrevious (st, Breadcrumb { parent, left, right } : bs)
    | Seq.null right = Left "Already at first scope."
    | otherwise      = Right (l, Breadcrumb parent left' (st <| right) : bs)
    where
        left' :> l = Seq.viewr left

goUp :: Zipper -> Either String Zipper
goUp (_, []) =
    Left "Already at root scope."
goUp (st, Breadcrumb { parent, left, right } : bs) =
    Right (SymbolTable parent ((left |> st) >< right), bs)

root :: Zipper -> Zipper
root (st, []) = (st, [])
root z        = root . (\(Right x) -> x) . goUp $ z

---- (de)focusing ------------
focus :: SymbolTable -> Zipper
focus = (,[])

defocus :: Zipper -> SymbolTable
defocus = fst

---- Using the table ---------
lookup :: String -> Zipper -> Either String Entry
lookup key (st, []) =
    lookup' key st
lookup key zipper@(st, _) =
    case lookup' key st of
        Left     _ -> goUp zipper >>= lookup key
        rightEntry -> rightEntry

local :: String -> Zipper -> Either String Entry
local key (st, _) =
    lookup' key st

insertSymbol :: String -> Entry -> Zipper -> Zipper
insertSymbol key entry (st, bs) =
    (insert' key entry st, bs)

insertST :: SymbolTable -> Zipper -> Zipper
insertST newST (st, bs) =
    (insertST' newST st, bs)

openScope :: (Int, Int) -> Zipper -> Zipper
openScope pos = (\(Right x) -> x) . goDownLast . insertST (empty' pos)

closeScope :: (Int, Int) -> Zipper -> Zipper
closeScope pos (st, bs) =
    (exit' pos st, bs)
