{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
import           Language.Epilog.Epilog
import           Language.Epilog.IR.Monad    hiding (symbols)
import           Language.Epilog.MIPS.Monad
import           Language.Epilog.IR.Program
import           Language.Epilog.IR.TAC
import           Language.Epilog.MIPS.Gmips  (Gmips (..))
import           Language.Epilog.MIPS.MIPS   (emitMIPS)
import           Language.Epilog.Parser
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Control.Lens                (makeLenses, (.~), (^.))
import qualified Data.Map                    as Map
import           Data.Text.IO                (writeFile)
import           Prelude                     hiding (writeFile)
import           System.Console.GetOpt       (ArgDescr (..), ArgOrder (..),
                                              OptDescr (..), getOpt, usageInfo)
import           System.Environment          (getArgs)
import           System.Exit                 (exitSuccess)
import           System.FilePath.Posix       ((-<.>))
import           System.IO                   (Handle, IOMode (ReadMode),
                                              hGetContents, openFile, stdin)
--------------------------------------------------------------------------------
-- Options -----------------------------
data Options = Options
  { _help    :: Bool
  , _version :: Bool
  , _action  :: FilePath -> Handle -> IO () }

makeLenses ''Options

defaultOptions :: Options
defaultOptions  = Options
  { _help     = False
  , _version  = False
  , _action   = doMIPS }

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
    "Performs lexical and syntactic analysis of the file"
  , Option ['i'] ["ir"]
    (NoArg (action .~ doIR))
    "Generates intermediate code for the file after performing lexical and syntactic analysis of it"
  , Option ['m'] ["mips"]
    (NoArg (action .~ doMIPS))
    "Generates MIPS code for the file after performing lexical and syntactic analysis of it" ]

helpStr :: String
helpStr = usageInfo message options

versionStr :: String
versionStr = "epilog 0.1.0.0"

message :: String
message =
  "usage: epilog [OPTION]... [FILE]\n\
  \\twhen running epilog without arguments, the compiler consumes data\n\
  \\tit receives from the standard input until it receives an EOF\n\
  \\t('^D') character.\n\
  \\n\
  \\tif more than one action is specified, only the last one is performed."

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

  (a, s, _w) <- runEpilog parse inp

  when (s^.parseOK) $ do
    putStrLn . drawTree . toTree $ a
    putStrLn ""

    putStrLn "Symbols:"
    putStrLn . drawTree . toTree . defocus $ s^.symbols
    putStrLn ""

    unless (null $ s^.types) $ do
      putStrLn "Types:"
      forM_ (Map.toList $ s^.types) $ \(name, (t, p)) ->
        putStrLn $ "`" <> name <> "` " <> show p <> " as\n" <>
            (drawTree . toTree $ t)
      putStrLn ""

    unless (null $ s^.strings) $ do
      putStrLn "Strings:"
      forM_ (Map.toList $ s^.strings) $ \(str, i) -> do
        putStrLn $ "#" <> show i <> ": " <> str
        -- mapM_ (\p -> putStrLn $ "\t" <> show p) ps

    -- unless (null errors) $ do
    --   hPutStrLn stderr "Errors:"
    --   mapM_ (hPrint stderr) errors
    --
    -- unless (null $ s^.ast) $ do
    --   putStrLn "AST:"
    --   mapM_ (\inst -> putStrLn $ drawTree . toTree $ inst) (s^.ast)
    --   putStrLn "\n"

doIR :: FilePath -> Handle -> IO ()
doIR filename handle = do
  inp <- hGetContents handle

  putStrLn $ unwords ["Generating IR for", filename]

  (ast, s, _w) <- runEpilog parse inp

  when (s^.parseOK) $ do
    (code, _s) <- runIR irProgram ast

    putStrLn $ emit code


doMIPS :: FilePath -> Handle -> IO ()
doMIPS filename handle = do
  inp <- hGetContents handle

  putStrLn $ unwords ["Generating MIPS for", filename]

  (ast, s, _w) <- runEpilog parse inp

  when (s^.parseOK) $ do
    (code, _s) <- runIR irProgram ast
    runMIPS gmips code >>= writeFile (filename -<.> "asm") . emitMIPS

-- Main --------------------------------
main :: IO ()
main = do
  (opts, args) <- getOpts

  when (opts^.version) doVersion
  when (opts^.help) doHelp

  (handle, filename) <- if null args
    then (, "<stdin>") <$> return stdin
    else (, head args) <$> openFile (head args) ReadMode

  (opts^.action) filename handle
