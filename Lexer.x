{
module Main (main) where

import Tokens (Position(..), Token(..))

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@id    =[A-Z][$alpha $digit \_ \’]*
@badid = $alpha[$alpha $digit \_ \’]*

tokens :-

  $white+     ;
  "%%".*      ;
  is          { \s -> TokenIs $ Position (1,1) }
  $digit+     { \s -> TokenInteger (read s) $ Position(1,1) }

  \".*\"      { \s -> TokenString ((tail.init) s) $ Position (1,1) }
  \".*        { \s -> TokenError "Unclosed string" $ Position (1,1) }

  @id         { \s -> TokenIdentifier s $ Position(1,1) }
  @badid      { \s -> TokenError ("Identifier " ++show s++" must be capitalized") $ Position (1,1)}
  .           { \s -> TokenError "Syntax Error" $ Position (1,1)}





{

main = do
  s <- getLine
  putStr $ unlines $ map ((++"\n").show) $ alexScanTokens s

}