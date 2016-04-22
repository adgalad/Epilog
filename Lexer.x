{
module Main (main) where

import Tokens (Position(..), Token(..))

}


%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

$special   = [\(\)\,\;\[\]\`\{\}\"] 
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

@octit     = 0[xX][0-7]+
@hexit     = 0[xX][$digit A-F a-f]+

@id        = [A-Z][$alpha $digit  \’ ]*
@badid     = $alpha[$alpha $digit \’ ]*
@string    = \".*\"
@badstring = \".*

@exponent    = [eE] [\-\+]? $digit+
@floating_point = $digit+ \. $digit+ @exponent? | $digit+ @exponent

tokens :-

  $white+     ;
  "%%".*      ;
  is          { buildToken TokenIs }
  "+"         { buildToken TokenPlus }
  "-"         { buildToken TokenMinus }
  "*"         { buildToken TokenTimes }
  "/"         { buildToken TokenDivision }

-- Integer (8,10,16) Float
  @floating_point { buildTokenWithValue (TokenFloat)   read }
  @octit          { buildTokenWithValue (TokenInteger) read }
  $digit+         { buildTokenWithValue (TokenInteger) read }
  @hexit          { buildTokenWithValue (TokenInteger) read }
  
  @string     { buildTokenWithValue TokenString (tail.init) }
  @badstring  { buildTokenWithValue TokenError ("Unclosed string: "++)  }

  @id         { buildTokenWithValue TokenIdentifier id }
  @badid      { buildTokenWithValue TokenError ("Identifier must be capitalized: "++) }
  .           { buildTokenWithValue TokenError ("Syntax Error: "++) }





{

-- Just returns the given token with its position
buildToken :: (Position -> Token) -> AlexPosn -> String -> Token
buildToken token (AlexPn _ line column) _ = token (Position (line,column))

-- Returns a token, builded with a value and its position. 
-- The value is obtained by applying a function to the string given by Alex
buildTokenWithValue :: (b -> Position -> Token) -> (String -> b) -> AlexPosn -> String -> Token
buildTokenWithValue token f (AlexPn _ line column) str = token (f str) (Position (line,column))


main = do
  s <- getLine
  putStr $ unlines $ map ((++"\n").show) $ alexScanTokens s

}