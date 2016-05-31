{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Main (main) where
--------------------------------------------------------------------------------
-- import           Language.Epilog.Context
import           Language.Epilog.Epilog
import           Language.Epilog.Parser
import           Language.Epilog.STTester
--------------------------------------------------------------------------------
import           Control.Lens             (makeLenses, (.~), (^.))
import           Control.Monad            (unless, when)
import           System.Console.GetOpt    (ArgDescr (..), ArgOrder (..),
                                           OptDescr (..), getOpt, usageInfo)
import           System.Environment       (getArgs)
import           System.Exit              (exitSuccess)
import           System.IO                (Handle, IOMode (ReadMode), hClose,
                                           hGetContents, hPutStrLn, openFile,
                                           stderr, stdin)
--------------------------------------------------------------------------------
-- Options -----------------------------
data Options = Options
    { _help    :: Bool
    , _version :: Bool
    , _action  :: FilePath -> Handle -> IO ()
    }

makeLenses ''Options

defaultOptions :: Options
defaultOptions  = Options
    { _help     = False
    , _version  = False
    , _action   = doParse
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (help .~ True))
        "shows this help message"
    , Option ['v'] ["version"]
        (NoArg (version .~ True))
        "shows version number"
    , Option ['p'] ["parse"]
        (NoArg (action .~ doParse))
        "Performs `lex` and syntactic analysis of the file"
    , Option ['s'] ["symTable"]
        (NoArg (action .~ doST))
        "Builds and shows the symbol table tree of the file"
    ]

helpStr :: String
helpStr = usageInfo message options

versionStr :: String
versionStr = "epilog 0.1.0.0"

message :: String
message = unlines
    [ "usage: epilog [OPTION]... [FILE]"
    , "\twhen running epilog without arguments, the compiler consumes data"
    , "\tit receives from the standard input until it receives an EOF"
    , "\t('^D') character."
    , ""
    , "\tif more than one action is specified, only the last one is performed."
    ]

getOpts :: IO (Options, [String])
getOpts = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, rest, []  ) ->
            return (foldl (flip id) defaultOptions flags, rest)
        (_, _, errs) ->
            ioError $ userError (concat errs ++ helpStr)

-- Actions -----------------------------
doVersion :: IO ()
doVersion = do
    putStrLn versionStr
    exitSuccess

doHelp :: IO ()
doHelp = do
    putStr helpStr
    exitSuccess

doParse :: FilePath -> Handle -> IO ()
doParse filename handle  = do
    inp <- hGetContents handle

    putStrLn $ unwords ["Parsing", filename]

    let (_a, _s, w) = runEpilog parse () (initialState inp)
    mapM_ (hPutStrLn stderr . show) w


    -- putStrLn "Symbols:"
    -- putStr . drawTree . toTree $ defocus $ symbols (fst p)
    -- putStrLn "Types:"
    -- mapM_ (\(name, (_, _, p)) ->
    --     putStrLn $ "\t" ++ name ++ " at " ++ showP p
    --     ) (Map.toList $ types (fst p))
    -- putStrLn $ show $ strings (fst p)
    -- mapM_ (hPrint stderr) (snd p)

    --when (null plerrs) $ putStrLn . drawTree $ toTree prog

doST :: FilePath -> Handle -> IO ()
doST filename handle = do
    putStrLn $ unwords
        [ "I will proceed to ignore", filename
        , "and show you an interactive prompt."
        , "I hope you're okay with this."
        ]

    unless (handle == stdin) (hClose handle)

    tester


-- Main --------------------------------
main :: IO ()
main = do
    (opts, args) <- getOpts

    when (opts^.version) doVersion
    when (   opts^.help) doHelp

    (handle, filename) <- if null args
        then (, "<stdin>") <$> return stdin
        else (, head args) <$> openFile (head args) ReadMode

    (opts^.action) filename handle


