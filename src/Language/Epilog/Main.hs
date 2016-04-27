{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main (main) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer

import           Control.Monad             (guard, void, when)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT)

import           System.IO                 (stderr, hPutStr)                         
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
    
    case sr of 
        Left a -> liftIO (error a)
        Right b -> do 
            let (tokens, errors) = split b 
            liftIO $ mapM_  ((hPutStr stderr) . niceShow) errors
            liftIO $ mapM_  (putStrLn . niceShow) errors
            where 
                split x = (getTokens x, getErrors x)
                getTokens [] = []
                getTokens (x:xs) = case x of 
                                        Lexeme _ (ErrorUnderflow _) -> getTokens xs
                                        Lexeme _ (ErrorOverflow _) -> getTokens xs
                                        Lexeme _ (ErrorUnclosedStringLiteral _)-> getTokens xs
                                        Lexeme _ (ErrorUnexpectedToken _) -> getTokens xs
                                        otherwise -> x:(getTokens xs)
                getErrors [] = []
                getErrors (x:xs) = case x of
                                        Lexeme _ (ErrorUnderflow _) -> x:(getErrors xs)
                                        Lexeme _ (ErrorOverflow _) -> x:(getErrors xs)
                                        Lexeme _ (ErrorUnclosedStringLiteral _)-> x:(getErrors xs)
                                        Lexeme _ (ErrorUnexpectedToken _) -> x:(getErrors xs) 
                                        otherwise -> getErrors xs
                                