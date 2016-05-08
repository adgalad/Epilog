module Language.Epilog.SymbolTable
    ( SymbolTable(..)
    , Zipper(..)
    , prueba
    ) where


--------------------------------------------------------------------------------
import           Language.Epilog.Treelike

import           Data.Sequence          hiding (null)
import qualified Data.Sequence as DS    (null)
import qualified Data.Map.Strict as DM  (Map, fromList, toList, lookup, null, insert, singleton, showTree)
import           Data.Maybe             (fromJust)
import           Prelude                hiding (lookup)
--------------------------------------------------------------------------------








data SymbolTable a = SymbolTable (DM.Map String a) (Forest_ a) deriving (Show)
data Breadcrumb  a = Breadcrumb  (DM.Map String a) (Forest_ a) (Forest_ a) deriving (Show)
type Forest_     a = Seq (SymbolTable a)
type Breadcrumbs a = [Breadcrumb a]
type Zipper      a = (SymbolTable a, Breadcrumbs a)

instance (Show a) => Treelike (SymbolTable a) where
    toTree (SymbolTable n childs) = Node "Scope" (dmToTree n:(toForest childs))

dmToTree :: (Show a, Show b) => DM.Map a b -> Tree String
dmToTree dm = Node "Identifiers" (map (((flip Node) []).showtuple) (DM.toList dm))
    where   showtuple :: (Show a, Show b) => (a,b) -> String 
            showtuple (a,b) = show a ++ " -> "++ show b 

{- Solo son pruebas -}
prueba :: String
prueba = drawTree $ toTree x

x = SymbolTable (DM.fromList [("ID 0.1", "Info 0.1"), ("ID 0.2", "Info 0.2") , ("ID 0.3", "Info 0.3")]) 
                (fromList [ SymbolTable (DM.singleton "ID 1.1" "Info 1.1") 
                                        (singleton (SymbolTable (DM.fromList [("ID 2.1", "Info 2.1"), ("ID 2.2", "Info 2.2") , ("ID 2.3", "Info 2.3")]) empty))
                          , SymbolTable (DM.singleton "ID 1.2" "Info 1.2") empty]
                )

y = SymbolTable (DM.singleton "ID 0.1" "Info 0.1") 
                (fromList [ SymbolTable (DM.singleton "ID 1.1" "Info 1.1") 
                                        (singleton (SymbolTable (DM.singleton "ID 0.1" "Info 0.1") empty))
                          , SymbolTable (DM.singleton "ID 1.1" "Info 1.1") 
                                        (singleton (SymbolTable (DM.singleton "ID 0.1" "Info 0.1") empty))]
                )
{- Fin de las Pruebas -}


goDown :: Zipper a -> Maybe (Zipper a)
goDown (SymbolTable n childs, bs) = if DS.null childs 
    then Nothing
    else Just (x, Breadcrumb n empty xs :bs)
        where x :< xs = viewl childs

goRight :: Zipper a -> Maybe (Zipper a)
goRight (st, bs) = case bs of 
    [] -> Nothing
    Breadcrumb p l r:bs' -> if DS.null r
        then Nothing
        else Just (x, Breadcrumb p (l |> st) xs :bs')
            where x :< xs = viewl r


goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (st, bs) = case bs of 
    [] -> Nothing
    Breadcrumb p l r:bs' -> if DS.null l
        then Nothing
        else Just (x, Breadcrumb p xs (st <| r) :bs')
            where xs :> x = viewr l

goBack :: Zipper a -> Maybe (Zipper a)
goBack (st, bs) = case bs of 
    [] -> Nothing
    Breadcrumb p l r:bs' -> 
        Just (SymbolTable p ((l |> st) >< r), bs')


tothetop :: Zipper a -> Zipper a
tothetop (st, []) = (st, [])
tothetop z = tothetop $ fromJust $ goBack z

lookup :: String -> Zipper a -> Maybe a
lookup key (SymbolTable n childs,bs) = case DM.lookup key n of 
    Nothing -> if null bs then Nothing
               else lookup key 
                    (fromJust $ goBack (SymbolTable n childs,bs))
    Just elem -> Just elem

insertElement ::  String -> a -> Zipper a -> Zipper a
insertElement key elem (SymbolTable n childs, bs) 
    = (SymbolTable (DM.insert key elem n) childs, bs)

insertSymTable :: SymbolTable a -> Zipper a -> Zipper a
insertSymTable newSt (SymbolTable n childs, bs) 
    = (SymbolTable n (childs |> newSt), bs)

focus :: SymbolTable a-> Zipper a
focus s = (s, [])

defocus :: Zipper a -> SymbolTable a
defocus (st,bs) = st 