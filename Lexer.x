{
module Main (main) where

import Tokens (Position(..), Token(..))

}


%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@id    = [A-Z][$alpha $digit  \’ ]*
@badid = $alpha[$alpha $digit \’ ]*

tokens :-

  $white+     ;
  "%%".*      ;
  is          { buildToken TokenIs }
  $digit+     { buildTokenWithValue (TokenInteger) read }

  \".*\"      { buildTokenWithValue TokenString (tail.init) }
  \".*        { buildTokenWithValue TokenError ("Unclosed string: "++)  }

  @id         { buildTokenWithValue TokenIdentifier id }
  @badid      { buildTokenWithValue TokenError ("Identifier must be capitalized: "++) }
  .           { buildTokenWithValue TokenError ("Syntax Error: "++) }





{

buildToken :: (Position -> Token) -> AlexPosn -> String -> Token
buildToken token (AlexPn _ line column) _ = token (Position (line,column))

buildTokenWithValue :: (b -> Position -> Token) -> (String -> b) -> AlexPosn -> String -> Token
buildTokenWithValue token f (AlexPn _ line column) value = token (f value) (Position (line,column))


main = do
  s <- getLine
  putStr $ unlines $ map ((++"\n").show) $ alexScanTokens s

}