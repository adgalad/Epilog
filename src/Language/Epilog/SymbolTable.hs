{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Language.Epilog.SymbolTable
    ( Entry (..)
    , Scope (..)
    , SymbolTable
    , closeScope
    , defocus
    , empty
    , emptyP
    , focus
    , goDownFirst
    , goDownLast
    , goNext
    , goPrevious
    , goUp
    , insertST
    , insertSymbol
    , isLocal
    , isSymbol
    , local
    , lookup
    , openScope
    , root
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.Treelike
import           Language.Epilog.Position
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
    , varPosition     :: Position
    } deriving (Eq)

instance Treelike Entry where
    toTree Entry { varName, varType, varInitialValue, varPosition } =
        Node ("Variable `" ++ varName ++ "`") $
            leaf ("Declared at " ++ show varPosition) :
            leaf ("Type: " ++ show varType) :
            case varInitialValue of
                Nothing -> [leaf "Not initialized"]
                Just e  -> [Node "Initialized with value" [toTree e]]

-- Symbol Table Scope ------------------
type Entries = Map String Entry

data Scope = Scope
    { sFrom     :: Position
    , sTo       :: Position
    , sEntries  :: Entries
    , sChildren :: Scopes
    }

type Scopes = Seq Scope

instance Treelike Scope where
    toTree Scope { sFrom, sTo, sEntries, sChildren } =
        Node ("Scope " ++ showP sFrom ++ " -> " ++ showP sTo) $
            (if Map.null sEntries
                then leaf "No symbols"
                else Node "Symbols" (toForest . Map.elems $ sEntries)) :
            toForest sChildren

lookup' :: String -> Scope -> Either String Entry
lookup' key Scope { sEntries } =
    case Map.lookup key sEntries of
         Just entry -> Right entry
         Nothing    -> Left "Not found."

insert' :: String -> Entry -> Scope -> Scope
insert' key entry s @ Scope { sEntries } =
    s { sEntries = Map.insert key entry sEntries }

insertST' :: Scope -> Scope -> Scope
insertST' newScope s @ Scope { sChildren } =
    s { sChildren = sChildren |> newScope }

close' :: Position -> Scope -> Scope
close' position scope =
    scope { sTo = position }

empty' :: Position -> Scope
empty' position = Scope
    { sFrom     = position
    , sTo       = position
    , sEntries  = Map.empty
    , sChildren = Seq.empty
    }

-- Symbol Table ------------------------
data Breadcrumb = Breadcrumb
    { bScope :: (Position, Position, Entries)
    , bLeft  :: Scopes
    , bRight :: Scopes
    }

type SymbolTable = (Scope, [Breadcrumb])

---- Starter Symbo lTable ----
empty :: SymbolTable
empty = focus $ empty' (Position (0, 0))

emptyP :: Position -> SymbolTable
emptyP = focus . empty'

---- Moving around -----------
goDownFirst :: SymbolTable -> Either String SymbolTable
goDownFirst (Scope { sFrom, sTo, sEntries, sChildren }, bs)
    | Seq.null sChildren = Left "No embedded scopes."
    | otherwise =
        Right (x, Breadcrumb (sFrom, sTo, sEntries) Seq.empty xs : bs)
    where
        x :< xs = Seq.viewl sChildren

goDownLast :: SymbolTable -> Either String SymbolTable
goDownLast (Scope { sFrom, sTo, sEntries, sChildren }, bs)
    | Seq.null sChildren = Left "No embedded scopes."
    | otherwise =
        Right (x, Breadcrumb (sFrom, sTo, sEntries) xs Seq.empty : bs)
    where
        xs :> x = Seq.viewr sChildren

goNext :: SymbolTable -> Either String SymbolTable
goNext (_, []) =
    Left "Root scope has no siblings."
goNext (s, Breadcrumb { bScope, bLeft, bRight } : bs)
    | Seq.null bRight = Left "Already at last scope."
    | otherwise =
        Right (r, Breadcrumb bScope (bLeft |> s) bRight' : bs)
    where
        r :< bRight' = Seq.viewl bRight

goPrevious :: SymbolTable -> Either String SymbolTable
goPrevious (_, []) =
    Left "Root scope has no siblings."
goPrevious (s, Breadcrumb { bScope, bLeft, bRight } : bs)
    | Seq.null bRight = Left "Already at first scope."
    | otherwise =
        Right (l, Breadcrumb bScope bLeft' (s <| bRight) : bs)
    where
        bLeft' :> l = Seq.viewr bLeft

goUp :: SymbolTable -> Either String SymbolTable
goUp (_, []) =
    Left "Already at root scope."
goUp (s, Breadcrumb { bScope = (sFrom, sTo, sEntries), bLeft, bRight } : bs) =
    Right (Scope sFrom sTo sEntries ((bLeft |> s) >< bRight), bs)

root :: SymbolTable -> Either a SymbolTable -- we want to stay in the monad
root (s, []) = Right (s, [])
root st      = root . (\(Right x) -> x) . goUp $ st

---- (de)focusing ------------
focus :: Scope -> SymbolTable
focus = (,[])

defocus :: SymbolTable -> Scope
defocus = fst

---- Using the table ---------
isSymbol :: String -> SymbolTable -> Bool
isSymbol key st = case lookup key st of
    Left _ -> False
    Right _ -> True

isLocal ::  String -> SymbolTable -> Bool
isLocal key st = case local key st of
    Left _ -> False
    Right _ -> True

lookup :: String -> SymbolTable -> Either String Entry
lookup key (s, []) =
    lookup' key s
lookup key st@(s, _) =
    case lookup' key s of
        Left      _ -> goUp st >>= lookup key
        bRightEntry -> bRightEntry

local :: String -> SymbolTable -> Either String Entry
local key (s, _) =
    lookup' key s

insertSymbol :: String -> Entry -> SymbolTable -> SymbolTable
insertSymbol key entry (s, bs) =
    (insert' key entry s, bs)

insertST :: Scope -> SymbolTable -> SymbolTable
insertST newST (s, bs) =
    (insertST' newST s, bs)

openScope :: Position -> SymbolTable -> SymbolTable
openScope p = (\(Right x) -> x) . goDownLast . insertST (empty' p)

closeScope :: Position -> SymbolTable -> SymbolTable
closeScope p (s, bs) =
    (close' p s, bs)
