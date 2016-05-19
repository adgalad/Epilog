module Language.Epilog.STTester
    ( tester
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.State   (StateT, execStateT, get, gets,
                                              modify, put, runStateT)
import           Prelude                     hiding (lookup)
import           System.IO                   (hFlush, stdout)
--------------------------------------------------------------------------------
----------- The code that follows is truly awful but it works, kinda -----------
-------------- It would probably be pretty cool with more monads ---------------

-- Utility Computations ----------------
say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

prompt :: MonadIO m => String -> m ()
prompt s = do
    liftIO . putStr $ s ++ " > "
    liftIO . hFlush $ stdout

doAux :: MonadIO m => m String
doAux = do
    prompt "Name?"
    line <- liftIO getLine
    case words line of
        [name] -> return name
        _      -> do
            say "Invalid name."
            doAux

doWhat :: MonadIO m => m ()
doWhat = say "I don't know how to do that :("

-- Tester ------------------------------
tester :: IO ()
tester = do
    putStrLn $ unlines
        [ "\nAvailable actions:"
        , "\tvar    - to declare a Variable"
        , "\topen   - to Open a new scope"
        , "\tclose  - to Close the current scope"
        , "\tfinish - to Finish the build process and print the Symbol Table"
        ]

    BuildState st _ <- execStateT builder initialBuildState
    putStrLn "The tree has been built."

    _ <- getChar -- wait for an enter keypress
    putStr . drawTree . toTree . defocus $ st

    _ <- getChar -- wait for an enter keypress
    putStrLn $ unlines
        [ "Available actions:"
        , "\tup       - to go Up a scope"
        , "\tdown     - to go Down into the first child scope"
        , "\tnext     - to go to the Next sibling scope"
        , "\tprevious - to go to the Previous sibling scope"
        , "\tlookup   - to Lookup the closest definition of a variable"
        , "\tlocal    - to find the Local definition of a variable"
        , "\tquit     - to Quit the tester"
        ]
    void $ runStateT explorer st

-- Builder -----------------------------
-- State ---------------------
data BuildState = BuildState
    { symbols  :: SymbolTable
    , position :: Position
    }

initialBuildState :: BuildState
initialBuildState = BuildState
    { symbols  = empty
    , position = Position (0,0)
    }

nextPos :: Position -> Position
nextPos (Position (r, c)) = Position (r+1, (c+2) `mod` 80)
nextPos p = p

-- Computations --------------
builder :: StateT BuildState IO ()
builder = do
    prompt "What to do?"
    line <- liftIO getLine
    case words line of
        ["var"      ] -> doAux >>= doVar
        ["var", name] -> doVar name
        ["open"     ] -> doOpen
        ["close"    ] -> doClose
        ["finish"   ] -> doFinish
        _             -> doWhat >> builder

doVar :: String -> StateT BuildState IO ()
doVar name = do
    st <- gets symbols
    case local name st of
        Left _ -> do
            p <- gets position

            say $ "OK. integer `" ++ name ++ "` declared at " ++ show p ++ "."

            let entry = Entry name intT Nothing p

            put BuildState
                { symbols  = insertSymbol name entry st
                , position = nextPos p
                }
        Right _ ->
            say $ "Variable `" ++ name ++
                "` already declared in this scope." ++
                "Not added to scope."
    builder

doOpen :: StateT BuildState IO ()
doOpen = do
    BuildState st p <- get

    say $ "OK. Scope opened at " ++ show p ++ "."

    put BuildState
        { symbols  = openScope p st
        , position = nextPos p
        }
    builder

doClose' :: StateT BuildState IO () -> StateT BuildState IO ()
doClose' c = do
    BuildState st p <- get
    let st' = closeScope p st

    say $ "OK. Scope closed at " ++ show p ++ "."

    case goUp st' of
        Left _ ->
            modify (\s -> s { symbols= st' })
        Right st'' -> do
            put BuildState
                { symbols  = st''
                , position = nextPos p
                }
            c

doClose :: StateT BuildState IO ()
doClose = doClose' builder -- first close the current scope, then keep building

doFinish :: StateT BuildState IO ()
doFinish = doClose' doFinish -- first close the current scope, then recurse

-- Explorer ----------------------------
-- Computations --------------
explorer :: StateT SymbolTable IO ()
explorer = do
    prompt "What to do?"
    line <- liftIO getLine
    case words line of
        ["up"          ] -> doMove goUp
        ["down"        ] -> doMove goDownFirst
        ["next"        ] -> doMove goNext
        ["previous"    ] -> doMove goPrevious

        ["lookup"      ] -> doAux >>= doFind lookup
        ["local"       ] -> doAux >>= doFind local

        ["lookup", name] -> doFind lookup name
        ["local" , name] -> doFind local name

        ["quit"        ] -> doQuit
        _                -> doWhat >> explorer

doMove :: (SymbolTable -> Either String SymbolTable) -> StateT SymbolTable IO ()
doMove f = do
    st <- get
    case f st of
        Left e   -> say e
        Right st' -> do
            say "OK."
            put st'
    explorer

doFind :: (String -> SymbolTable -> Either String Entry) -> String
       -> StateT SymbolTable IO ()
doFind f name = do
    st <- get
    case f name st of
        Left e  -> say e
        Right r -> say . drawTree . toTree $ r
    explorer

doQuit :: StateT SymbolTable IO ()
doQuit = say "Bye."
