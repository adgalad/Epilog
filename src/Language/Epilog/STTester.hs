module Language.Epilog.STTester
    ( tester
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Monad               (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Prelude                     hiding (lookup)
import           System.IO                   (hFlush, stdout)
--------------------------------------------------------------------------------
----------- The code that follows is truly awful but it works, kinda -----------
-------------- It would probably be pretty cool with more monads ---------------

-- Utility Computations ----------------
prompt :: String -> StateT a IO ()
prompt s = do
    liftIO . putStr $ s ++ " > "
    liftIO . hFlush $ stdout

ok :: StateT a IO ()
ok =
    liftIO . putStrLn $ "Ok."

doAux :: (String -> StateT a IO ()) -> StateT a IO ()
doAux c = do
    prompt "Name?"
    name <- liftIO getLine
    c name

doWhat :: StateT a IO () -> StateT a IO ()
doWhat c = do
    liftIO . putStrLn $ "I don't know how to do that :("
    c

-- Tester ------------------------------
tester :: IO ()
tester = do
    putStrLn $ unlines
        [ "\nAvailable actions:"
        , "\tVar    - to declare a Variable"
        , "\tOpen   - to Open a new scope"
        , "\tClose  - to Close the current scope"
        , "\tFinish - to Finish the build process, printing the resulting Symbol Table"
        , "The caps are important!"
        ]
    BuildState z _ <- execStateT builder initialBuildState
    liftIO . putStrLn $ "The tree has been built."
    putStrLn . drawTree . toTree . defocus $ z
    putStrLn $ unlines
        [ "Available actions:"
        , "\tUp       - to go Up a scope"
        , "\tDown     - to go Down into the first child scope"
        , "\tNext     - to go to the Next sibling scope"
        , "\tPrevious - to go to the Previous sibling scope"
        , "\tLookup   - to find the closest definition of a variable if it exists"
        , "\tLocal    - to find the local definition of a variable if it exists"
        , "\tQuit     - to Quit the tester"
        , "The caps are important!"
        ]
    void $ runStateT explorer z

-- Builder -----------------------------
-- Actions -------------------
data BuilderAction
    = Var
    | Open
    | Close
    | Finish
    deriving (Eq, Show, Read)

-- State ---------------------
data BuildState = BuildState
    { zipper :: Zipper
    , pos    :: (Int, Int)
    }

initialBuildState :: BuildState
initialBuildState = BuildState
    { zipper = focus $ emptyST (0,0)
    , pos    = (0,0)
    }

-- Computations --------------
builder :: StateT BuildState IO ()
builder = do
    prompt "What to do?"
    line <- liftIO getLine
    case reads line of
        [(Var   ,       "")] -> doAux doVar
        [(Var   ,      " ")] -> doAux doVar
        [(Var   , ' ':name)] -> doVar name
        [(Open  ,       "")] -> doOpen
        [(Close ,       "")] -> doClose
        [(Finish,       "")] -> doFinish
        _                    -> doWhat builder

doVar :: String -> StateT BuildState IO ()
doVar name = do
    z <- gets zipper
    case local name z of
        Nothing -> do
            ok

            p@(r, c) <- gets pos
            let entry = Entry name intT Nothing p

            put BuildState
                { zipper = insertSymbol name entry z
                , pos    = (r+1, (c+2) `mod` 80)
                }
        Just _ ->
            liftIO . putStrLn $
                "Variable `" ++ name ++ "` already declared in this scope." ++
                "Not added to scope."
    builder

doOpen :: StateT BuildState IO ()
doOpen = do
    ok

    BuildState z (r,c) <- get

    put BuildState
        { zipper = openScope (r, c) z
        , pos = (r+1, (c+2) `mod` 80)
        }
    builder

doClose :: StateT BuildState IO ()
doClose = do
    BuildState z (r,c) <- get

    let z' = closeScope (r, c) z

    ok
    case goBack z' of
        Nothing ->
            modify (\s -> s {zipper = z'})
        Just z'' -> do
            put BuildState
                { zipper = z''
                , pos = (r+1, (c+2) `mod` 80)
                }
            builder

doFinish :: StateT BuildState IO ()
doFinish = do
    BuildState z (r, c) <- get
    let z' = closeScope (r, c) z
    case goBack z' of
        Nothing -> do
            ok
            modify (\s -> s {zipper = z'})
        Just _ ->
            doFinish

-- Explorer ----------------------------
-- Actions -------------------
data ExplorerAction
    = Up
    | Down
    | Next
    | Previous
    | Lookup
    | Local
    | Quit
    deriving (Eq, Show, Read)

-- Computations --------------
explorer :: StateT Zipper IO ()
explorer = do
    prompt "What to do?"
    line <- liftIO getLine
    case reads line of
        [(Up      ,       "")] -> doUp
        [(Down    ,       "")] -> doDown
        [(Next    ,       "")] -> doNext
        [(Previous,       "")] -> doPrevious
        [(Lookup  ,       "")] -> doAux doLookup
        [(Lookup  ,      " ")] -> doAux doLookup
        [(Lookup  , ' ':name)] -> doLookup name
        [(Local   ,       "")] -> doAux doLocal
        [(Local   ,      " ")] -> doAux doLocal
        [(Local   , ' ':name)] -> doLocal name
        [(Quit    ,       "")] -> doQuit
        _                      -> doWhat explorer

doUp :: StateT Zipper IO ()
doUp = do
    z <- get
    case goBack z of
        Nothing -> liftIO . putStrLn $ "Already at root scope"
        Just z' -> do
            ok
            put z'
    explorer

doDown :: StateT Zipper IO ()
doDown = do
    z <- get
    case goDownFirst z of
        Nothing -> liftIO . putStrLn $ "No embedded scopes"
        Just z' -> do
            ok
            put z'
    explorer

doNext :: StateT Zipper IO ()
doNext = do
    z <- get
    case goRight z of
        Nothing -> liftIO . putStrLn $ "Already at last scope"
        Just z' -> do
            ok
            put z'
    explorer

doPrevious :: StateT Zipper IO ()
doPrevious = do
    z <- get
    case goLeft z of
        Nothing -> liftIO . putStrLn $ "Already at first scope"
        Just z' -> do
            ok
            put z'
    explorer

doLookup :: String -> StateT Zipper IO ()
doLookup name = do
    z <- get
    case lookup name z of
        Nothing -> liftIO . putStrLn $ "Not found"
        Just entry -> liftIO . putStrLn . drawTree . toTree $ entry
    explorer

doLocal :: String -> StateT Zipper IO ()
doLocal name = do
    z <- get
    case local name z of
        Nothing -> liftIO . putStrLn $ "Not found"
        Just entry -> liftIO . putStrLn . drawTree . toTree $ entry
    explorer

doQuit :: StateT Zipper IO ()
doQuit =
    liftIO . putStrLn $ "Bye."
