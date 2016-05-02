{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer
import           Language.Epilog.Parser
import           Language.Epilog.Treelike

import           Control.Monad             (guard, void, when)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import           Data.List                 (nub)

import           Prelude                   hiding (null)
import qualified Prelude                   as P (null)

import           System.Console.GetOpt     (ArgDescr (..), ArgOrder (..),
                                            OptDescr (..), getOpt, usageInfo)
import           System.Environment        (getArgs)
import           System.IO                 (hPrint, stderr)
--------------------------------------------------------------------------------

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
    ]

data Flag
  = Help
  | Version
  | Lex
  | Parse
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]    (NoArg Help)
        "shows this help message"
    , Option ['v'] ["version"] (NoArg Version)
        "shows version number"
    , Option ['l'] ["lex"]     (NoArg Lex)
        "Performs the lexical analysis of the file"
    , Option ['p'] ["parse"]   (NoArg Parse)
        "Performs the lexical and syntactic analysis of the file"
    ]

opts :: IO ([Flag], [String])
opts = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, rest, []  ) -> return (nub flags, rest)
        (_    , _   , errs) -> ioError (userError (concat errs ++ help))

main :: IO ()
main = void $ runMaybeT $ do
    (flags, args) <- liftIO opts

    when (Version `elem` flags) doVersion
    when (Help    `elem` flags) doHelp

    guard $ not $ (Help `elem` flags) || (Version `elem` flags)

    (input, file) <- if P.null args
    then (, "<stdin>") <$> liftIO getContents
    else (, head args) <$> liftIO (readFile $ head args)

    (if Lex `elem` flags
        then doLex
        else doParse) input file


doVersion :: MaybeT IO ()
doVersion = liftIO $ putStrLn version

doHelp :: MaybeT IO ()
doHelp = liftIO $ putStr message

doLex :: String -> String -> MaybeT IO ()
doLex input file = do
    liftIO . putStrLn $ unwords ["Lexing", file]

    case scanner input of
        Left msg -> liftIO (error msg)
        Right tokens -> liftIO $ mapM_ split tokens
            where
                split l@(t :@ _) =
                    (if isError t
                        then hPrint stderr
                        else print) l

doParse :: String -> String -> MaybeT IO ()
doParse input file = do
    liftIO . putStrLn $ unwords ["Parsing", file]

    let (prog, plerrs) = parseProgram input
    liftIO . putStrLn . drawTree $toTree prog
