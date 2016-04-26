{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer

import           Control.Monad             (guard, void, when)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT)

import           Data.List                 (nub)

import           Prelude                   hiding (null)
import qualified Prelude                   as P (null)

import           System.Console.GetOpt     (ArgDescr (..), ArgOrder (..),
                                            OptDescr (..), getOpt, usageInfo)
import           System.Environment        (getArgs)
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

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]    (NoArg Help)    "shows this help message"
    , Option ['v'] ["version"] (NoArg Version) "shows version number"
    ]

data Flag
  = Help    -- -h | --help
  | Version -- -v | --version
  deriving (Show, Eq)

opts :: IO ([Flag], [String])
opts = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, rest, []  ) -> return (nub flags, rest)
        (_    , _   , errs) -> ioError (userError (concat errs ++ help))

main :: IO ()
main = void $ runMaybeT $ do
    (flags, args) <- liftIO opts

    when (Version `elem` flags) . liftIO $ putStrLn version
    when (Help    `elem` flags) . liftIO $ putStr message

    guard $ not $ (Help `elem` flags) || (Version `elem` flags)

    (input, file) <- if P.null args
    then (, "<stdin>") <$> liftIO getContents
    else (, head args) <$> liftIO (readFile $ head args)

    liftIO . putStrLn $
        ("Beginning analysis of the Epilog program in file " ++ file)

    let sr = scanner input
    either
        (liftIO . error)
        (liftIO . mapM_ (putStrLn . niceShow))
        sr
