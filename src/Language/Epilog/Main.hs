{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Context
import           Language.Epilog.Lexer
import           Language.Epilog.Parser
import           Language.Epilog.STTester
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
import           Language.Epilog.AST.Type
--------------------------------------------------------------------------------
import           Control.Monad               (unless, when)
import qualified Data.Map                    as Map (null, toList)
import qualified Data.Sequence               as Seq (null)
import           System.Console.GetOpt       (ArgDescr (..), ArgOrder (..),
                                              OptDescr (..), getOpt, usageInfo)
import           System.Environment          (getArgs)
import           System.Exit                 (exitSuccess)
import           System.IO                   (Handle, IOMode (ReadMode), hClose,
                                              hGetContents, hPrint, hPutStrLn,
                                              openFile, stderr, stdin)
--------------------------------------------------------------------------------
-- Options -----------------------------
version :: String
version = "epilog 0.1.0.0"

help :: String
help = usageInfo message options

message :: String
message = unlines
    [ "usage: epilog [OPTION]... [FILE]"
    , "\twhen running epilog without arguments, the compiler consumes data"
    , "\tit receives from the standard input until it receives an EOF"
    , "\t('^D') character."
    , ""
    , "\tif more than one action is specified, only the last one is performed."
    ]

data Options = Options
    { optHelp    :: Bool
    , optVersion :: Bool
    , optAction  :: Handle -> String -> IO ()
    }

defaultOptions  :: Options
defaultOptions   = Options
    { optHelp    = False
    , optVersion = False
    , optAction  = doContext
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\o -> o { optHelp = True }))
        "shows this help message"
    , Option ['v'] ["version"]
        (NoArg (\o -> o { optVersion = True }))
        "shows version number"
    , Option ['l'] ["lex"]
        (NoArg (\o -> o { optAction = doLex }))
        "Performs the lexical analysis of the file"
    , Option ['p'] ["parse"]
        (NoArg (\o -> o { optAction = doParse }))
        "Performs `lex` and syntactic analysis of the file"
    , Option ['s'] ["symTable"]
        (NoArg (\o -> o { optAction = doST }))
        "Builds and shows the symbol table tree of the file"
    , Option ['c'] ["context"]
        (NoArg (\o -> o { optAction = doContext }))
        "Performs `parse` and checks the usage of variables in the file"
    ]

getOpts :: IO (Options, [String])
getOpts = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, rest, []  ) ->
            return (foldl (flip id) defaultOptions flags, rest)
        (_, _, errs) ->
            ioError $ userError (concat errs ++ help)

-- Actions -----------------------------
doVersion :: IO ()
doVersion = do
    putStrLn version
    exitSuccess

doHelp :: IO ()
doHelp = do
    putStr help
    exitSuccess

doLex :: Handle -> String -> IO ()
doLex handle filename = do
    input <- hGetContents handle

    putStrLn $ unwords ["Lexing", filename]
    case scanner input of
        Left  msg    -> error msg
        Right tokens -> mapM_ split tokens

    where
        split l@(t :@ _) =
            (if isError t
                then hPrint stderr
                else print) l

doParse :: Handle -> String -> IO ()
doParse handle filename = do
    input <- hGetContents handle

    putStrLn $ unwords ["Parsing", filename]

    let p =  parseProgram input

    putStrLn "Symbols:"
    putStr . drawTree . toTree $ defocus $ symbols (fst p)
    putStrLn "Types:"
    mapM_ (\(name, (_, _, p)) ->
        putStrLn $ "\t" ++ name ++ " at " ++ showP p
        ) (Map.toList $ types (fst p))
    putStrLn $ show $ strings (fst p)
    mapM_ (hPrint stderr) (snd p)

    --when (null plerrs) $ putStrLn . drawTree $ toTree prog

doST :: Handle -> String -> IO ()
doST handle filename = do
    putStrLn $ unwords
        [ "I will proceed to ignore", filename
        , "and show you an interactive prompt."
        , "I hope you're okay with this."
        ]

    unless (handle == stdin) (hClose handle)

    tester

doContext :: Handle -> String -> IO ()
doContext handle filename = do
    print "usa -p"
--doContext handle filename = do
--    input <- hGetContents handle
--    putStrLn $ unwords ["Checking the contexts of", filename]

--    let (prog, plerrs) = parseProgram input
--    when (null plerrs) $ do
--        let (symbols, strings, types, procs, errors) = context prog
--        putStrLn "Symbols:"
--        putStr . drawTree . toTree $ defocus symbols

--        unless (Map.null strings) $ do
--            putStrLn "Strings:"
--            mapM_ (\(s, ps) -> do
--                print s
--                mapM_ (\p -> putStrLn $ "\t" ++ show p) ps
--                ) (Map.toList strings)

--        unless (Map.null types) $ do
--            putStrLn "Types:"
--            mapM_ (\(name, (_, _, p)) ->
--                putStrLn $ "\t" ++ name ++ " at " ++ showP p
--                ) (Map.toList types)

--        unless (Map.null procs) $ do
--            putStrLn "Procs:"
--            mapM_ (\(name, ProcSignature _ t p) ->
--                putStrLn $
--                    "\t" ++ name ++ "->" ++ typeName t ++ " at " ++ showP p
--                ) (Map.toList procs)

--        unless (Seq.null errors) $ do
--            hPutStrLn stderr "Errors:"
--            mapM_ (hPrint stderr) errors


-- Main --------------------------------
main :: IO ()
main = do

    (opts, args) <- getOpts

    when (optVersion opts) doVersion
    when (optHelp    opts) doHelp

    (handle, filename) <- if null args
        then (, "<stdin>") <$> return stdin
        else (, head args) <$> openFile (head args) ReadMode

    optAction opts handle filename
