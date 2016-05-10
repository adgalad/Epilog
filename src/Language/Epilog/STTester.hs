module Language.Epilog.STTester
    ( startTester
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import qualified Data.Sequence               as Seq (empty)
import           System.IO                   (hFlush, stdout)
--------------------------------------------------------------------------------
----------- The code that follows is truly awful but it works, kinda -----------
----------------- It would probably be pretty cool with monads -----------------

data Action
    = Var
    | Open
    | Close
    | Quit
    deriving (Eq, Show, Read)

startTester :: IO ()
startTester = do
    putStrLn $ unlines
        [ "\nAvailable actions:"
        , "\tVar   - to declare a Variable"
        , "\tOpen  - to Open a new scope"
        , "\tClose - to Close the current scope"
        , "\tQuit  - to exit, printing the final Symbol Table"
        , "The caps are important!"
        ]
    tester (focus (emptyST (0,0))) 0 0

tester :: Zipper -> Int -> Int -> IO ()
tester z r c = do
    putStr "What to do? > "
    hFlush stdout
    line <- getLine
    (case reads line of
        [(Var  ,       "")] -> doVarAux
        [(Var  ,      " ")] -> doVarAux
        [(Var  , ' ':name)] -> doVar name
        [(Open ,       "")] -> doOpen
        [(Close,       "")] -> doClose
        [(Quit ,       "")] -> doQuit
        _                    -> doWhat) z r c

doVar :: String -> Zipper -> Int -> Int -> IO ()
doVar name z r c =
    case local name z of
        Nothing -> do
            putStrLn "OK."
            tester
                (insertSymbol name entry z)
                (r+1) (c+2 `mod` 80)
        Just _ -> do
            putStrLn $
                "Variable `" ++ name ++ "` already declared in this scope." ++
                "Not added to scope."
            tester z r c
    where
        entry = Entry name (Type IntT Seq.empty) Nothing (r, c)

doVarAux :: Zipper -> Int -> Int -> IO ()
doVarAux z r c = do
    putStr "Name? > "
    hFlush stdout
    name <- getLine
    doVar name z r c

doOpen :: Zipper -> Int -> Int -> IO ()
doOpen z r c = do
    let z' = openScope (r, c) z
    putStrLn "OK."
    tester z' (r+1) (c+2 `mod` 80)

doClose :: Zipper -> Int -> Int -> IO ()
doClose z r c = do
    let z' = closeScope (r, c) z
    case goBack z' of
        Nothing -> do
            putStrLn "OK. Closed root scope."
            doPrint z'
        Just z'' -> do
            putStrLn "OK."
            tester z'' (r+1) (c+2 `mod` 80)

doQuit :: Zipper -> Int -> Int -> IO ()
doQuit z r c = do
    let z' = closeScope (r, c) z
    case goBack z' of
        Nothing -> do
            putStrLn "Ok. Quitting..."
            doPrint z'
        Just z'' ->
            doQuit z'' (r+1) (c+2 `mod` 80)


doPrint :: Zipper -> IO ()
doPrint = putStrLn . drawTree . toTree . defocus

doWhat :: Zipper -> Int -> Int -> IO ()
doWhat z r c = do
    putStrLn "I cannot do that :("
    tester z r c
