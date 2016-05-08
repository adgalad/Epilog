module Language.Epilog.SymbolTable
    ( SymTable(..)
    , Zipper(..)
    , buildSymTable
    ) where


--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Type
import           Language.Epilog.Parser
import           Language.Epilog.Treelike


import qualified Data.Map.Strict        as DM 
import           Data.Maybe             (fromJust)
import           Data.Sequence          hiding (null)
import qualified Data.Sequence          as DS (null)

--------------------------------------------------------------------------------


data SymTable a = SymTable
        (DM.Map String a) 
        (SymTables a) 
        deriving (Show)

data Breadcrumb a = Breadcrumb 
        (DM.Map String a) 
        (SymTables a) 
        (SymTables a) 
        deriving (Show)

type SymTables  a = Seq (SymTable a)
type Zipper     a = (SymTable a, [Breadcrumb a])

instance (Show a) => Treelike (SymTable a) where
    toTree (SymTable dm childs) = Node "Table" (dmToTree dm:(toForest childs))
        where 
            dmToTree :: (Show b) => DM.Map String b -> Tree String
            dmToTree dm = case list of 
                [] -> Node "No Symbols" []
                _  -> Node "Symbols" $
                        map (((flip Node) []).showtuple) list
                where list = (DM.toList dm)
            showtuple :: (Show b) => (String,b) -> String 
            showtuple (a,b) = a ++ " -> " ++ show b

{- Que lleva la tabla adentro (??) D: -}
data SymInfo = SymInfo 
    { value   :: Expression
    , getType :: Type
    }


{- Solo son pruebas -}
x = SymTable (DM.fromList [ ("ID 0.1", "Info 0.1")
                          , ("ID 0.2", "Info 0.2") 
                          , ("ID 0.3", "Info 0.3")]) 
                (fromList [ SymTable (DM.singleton "ID 1.1" "Info 1.1") 
                                     (singleton $ SymTable (DM.fromList [ ("ID 2.1", "Info 2.1")
                                                                        , ("ID 2.2", "Info 2.2") 
                                                                        , ("ID 2.3", "Info 2.3")]) 
                                                             (singleton $ SymTable DM.empty empty))
                          , SymTable (DM.singleton "ID 1.2" "Info 1.2") empty]
                )

y = SymTable (DM.singleton "ID 0.1" "Info 0.1") 
                (fromList [ SymTable (DM.singleton "ID 1.1" "Info 1.1") 
                                        (singleton (SymTable (DM.singleton "ID 0.1" "Info 0.1") empty))
                          , SymTable (DM.singleton "ID 1.1" "Info 1.1") 
                                        (singleton (SymTable (DM.singleton "ID 0.1" "Info 0.1") empty))]
                )
{- Fin de las Pruebas -}


goDown :: Zipper a -> Maybe (Zipper a)
goDown (SymTable dm childs, bs) = if DS.null childs 
    then Nothing
    else Just (x, Breadcrumb dm empty xs :bs)
        where x :< xs = viewl childs

goRight :: Zipper a -> Maybe (Zipper a)
goRight (st, bs) = case bs of 
    []                   -> Nothing
    Breadcrumb p l r:bs' -> if DS.null r
        then Nothing
        else Just (x, Breadcrumb p (l |> st) xs :bs')
            where x :< xs = viewl r


goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (st, bs) = case bs of 
    []                   -> Nothing
    Breadcrumb p l r:bs' -> if DS.null l
        then Nothing
        else Just (x, Breadcrumb p xs (st <| r) :bs')
            where xs :> x = viewr l

goBack :: Zipper a -> Maybe (Zipper a)
goBack (st, bs) = case bs of 
    []                   -> Nothing
    Breadcrumb p l r:bs' -> 
        Just (SymTable p ((l |> st) >< r), bs')


top :: Zipper a -> Zipper a
top (st, []) = (st, [])
top z = top.fromJust.goBack $ z

lookupSym :: String -> Zipper a -> Maybe a
lookupSym key (SymTable dm childs,bs) = case DM.lookup key dm of 
    Nothing -> if null bs then Nothing
               else lookupSym key 
                    $fromJust $goBack (SymTable dm childs,bs)
    symbol  -> symbol

insertSymbol ::  String -> a -> Zipper a -> Zipper a
insertSymbol key sym (SymTable dm childs, bs) 
    = (SymTable (DM.insert key sym dm) childs, bs)

insertSymTable :: SymTable a -> Zipper a -> Zipper a
insertSymTable newSt (SymTable dm childs, bs) 
    = (SymTable dm (childs |> newSt), bs)

focus :: SymTable a-> Zipper a
focus s = (s, [])

defocus :: Zipper a -> SymTable a
defocus (st,bs) = st 


buildSymTable :: String -> (SymTable String, String)
buildSymTable input = if null input then (x,"No hay WARNING :D") else (x, [])

