{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.Epilog
import           Language.Epilog.Parser
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Lens                (makeLenses, (.~), (^.))
import           Control.Monad               (unless, when)
import qualified Data.Map                    as Map
import           System.Console.GetOpt       (ArgDescr (..), ArgOrder (..),
                                              OptDescr (..), getOpt, usageInfo)
import           System.Environment          (getArgs)
import           System.Exit                 (exitSuccess)
import           System.IO                   (Handle, IOMode (ReadMode), hClose,
                                              hGetContents, hPrint, hPutStrLn,
                                              openFile, stderr, stdin)
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
            ioError $ userError (concat errs <> helpStr)

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

    (a, s, _w) <- runEpilog inp parse

    putStrLn . drawTree . toTree $ a
    putStrLn ""

    putStrLn "Symbols:"
    putStrLn . drawTree . toTree . defocus $ s^.symbols
    putStrLn ""

    unless (Map.null $ s^.types) $ do
        putStrLn "Types:"
        mapM_ (\(name, (t, p)) ->
            putStrLn $ "`" <> name <> "` " <> show p <> " as\n" <>
                (drawTree . toTree $ t)
            ) (Map.toList $ s^.types)
        putStrLn ""

    unless (Map.null $ s^.strings) $ do
        putStrLn "Strings:"
        mapM_ (\(str, ps) -> do
            print str
            mapM_ (\p -> putStrLn $ "\t" <> show p) ps
            ) (Map.toList $ s^.strings)

    -- unless (Seq.null errors) $ do
    --     hPutStrLn stderr "Errors:"
    --     mapM_ (hPrint stderr) errors

    -- unless (Seq.null $ s^.ast) $ do
    --     putStrLn "AST:"
    --     mapM_ (\inst -> putStrLn $ drawTree . toTree $ inst) (s^.ast)
    --     putStrLn "\n"


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
