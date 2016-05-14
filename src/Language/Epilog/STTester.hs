module Language.Epilog.STTester
    ( tester
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Trans.State   (StateT, execStateT, get, gets,
                                              modify, put, runStateT)

import qualified Data.Sequence               as S
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

    BuildState z _ <- execStateT builder initialBuildState
    putStrLn "The tree has been built."

    _ <- getChar -- wait for an enter keypress
    putStr . drawTree . toTree . defocus $ z

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
    void $ runStateT explorer z

-- Builder -----------------------------
-- State ---------------------
data BuildState = BuildState
    { zipper :: Zipper
    , pos    :: (Int, Int)
    }

initialBuildState :: BuildState
initialBuildState = BuildState
    { zipper = empty
    , pos    = (0,0)
    }

nextPos :: (Int, Int) -> (Int, Int)
nextPos (r, c) = (r+1, (c+2) `mod` 80)

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
    z <- gets zipper
    case local name z of
        Left _ -> do
            p <- gets pos

            say $ "OK. integer `" ++ name ++ "` declared at " ++ show p ++ "."

            let entry = Entry name (Type "integer" S.empty) Nothing p

            put BuildState
                { zipper = insertSymbol name entry z
                , pos    = nextPos p
                }
        Right _ ->
            liftIO . putStrLn $
                "Variable `" ++ name ++ "` already declared in this scope." ++
                "Not added to scope."
    builder

doOpen :: StateT BuildState IO ()
doOpen = do
    BuildState z p <- get

    say $ "OK. Scope opened at " ++ show p ++ "."

    put BuildState
        { zipper = openScope p z
        , pos = nextPos p
        }
    builder

doClose' :: StateT BuildState IO () -> StateT BuildState IO ()
doClose' c = do
    BuildState z p <- get
    let z' = closeScope p z

    say $ "OK. Scope closed at " ++ show p ++ "."

    case goUp z' of
        Left _ ->
            modify (\s -> s { zipper = z' })
        Right z'' -> do
            put BuildState
                { zipper = z''
                , pos = nextPos p
                }
            c

doClose :: StateT BuildState IO ()
doClose = doClose' builder -- first close the current scope, then keep building

doFinish :: StateT BuildState IO ()
doFinish = doClose' doFinish -- first close the current scope, then recurse

-- Explorer ----------------------------
-- Computations --------------
explorer :: StateT Zipper IO ()
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

doMove :: (Zipper -> Either String Zipper) -> StateT Zipper IO ()
doMove f = do
    z <- get
    case f z of
        Left e   -> liftIO . putStrLn $ e
        Right z' -> do
            say "OK."
            put z'
    explorer

doFind :: (String -> Zipper -> Either String Entry) -> String
       -> StateT Zipper IO ()
doFind f name = do
    z <- get
    case f name z of
        Left e  -> say e
        Right r -> say . drawTree . toTree $ r
    explorer

doQuit :: StateT Zipper IO ()
doQuit = say "Bye."
