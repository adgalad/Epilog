{ {-# OPTIONS_GHC -w #-}
  {-# LANGUAGE MultiWayIf #-}
module Language.Epilog.Lexer
    ( Alex (..)
    , Token (..)
    , Lexeme (..)
    , alexMonadScan
    , isError
    , runAlex'
    , scanner
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexeme
import           Language.Epilog.Token

import           Numeric.Limits         (minValue, maxValue)
import           Data.Int               (Int32)
import           Control.Monad          (liftM, when)
import           Data.Maybe             (fromJust, isJust)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq (empty, (|>))
--------------------------------------------------------------------------------
}

%wrapper "monadUserState"

$octit       = [0-7]
$digit       = [0-9]
$hexit       = [0-9 A-F a-f]

@octal       = 0[oO] $octit+
@decimal     = \-?$digit+
@hexadecimal = 0[xX] $hexit+

@exponent    = [eE][\-\+]? $digit+
@float       = @decimal \. $digit+ @exponent?

$upper       = [A-Z]
$lower       = [a-z]
$alpha       = [$upper $lower]

$idchar      = [$alpha $digit \']

@varid       = $upper $idchar*
@genid       = $lower $idchar*

$symbol      = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\^\|\-\~\(\)\,\:\;\[\]\`\{\}\ ]
$graphic     = [$alpha $digit $symbol]

$charesc     = [0nt\\\'\"]
@escape      = \\ ($charesc)

@charval     = ($graphic | @escape)
@stringval   = @charval*

@char        = \' @charval \'
@string      = \" @stringval \"
@badstring   = \" @stringval

--------------------------------------------------------------------------------
epilog :-

    -- Whitespace
    <0> $white+         ;

    -- Comments
    <0> "%%".*          ;
    <0> "/%"            { enterNewComment `andBegin` c }
    <c> "/%"            { embedComment }
    <c> "%/"            { unembedComment }
    <c> .               ;
    <c> "\n"            { skip }

    -- Logical Operators
    <0> "and"           { make TokenAnd }
    <0> "andalso"       { make TokenAndalso }
    <0> "or"            { make TokenOr }
    <0> "orelse"        { make TokenOrelse }
    <0> "not"           { make TokenNot }

    -- Bitwise Operations
    <0> "band"          { make TokenBand }
    <0> "bnot"          { make TokenBnot }
    <0> "bor"           { make TokenBor }
    <0> "bsl"           { make TokenBsl }
    <0> "bsr"           { make TokenBsr }
    <0> "bxor"          { make TokenBxor }

    -- Array
    <0> "length"        { make TokenLength }
    <0> ":"             { make TokenColon }

    -- Arithmetic Operators
    <0> "+"             { make TokenPlus }
    <0> "-"             { make TokenMinus }
    <0> "*"             { make TokenTimes }
    <0> "/"             { make TokenFloatDivision }
    <0> "div"           { make TokenIntegerDivision }
    <0> "rem"           { make TokenRem }

    -- Relational
    <0> "<"             { make TokenLT }
    <0> "=<"            { make TokenLTE }
    <0> ">"             { make TokenGT }
    <0> ">="            { make TokenGTE }
    <0> "|"             { make TokenFactorOf }

    -- Equality
    <0> "="             { make TokenEQ }
    <0> "/="            { make TokenNE }

    -- Control Structures
    <0> "end"           { make TokenEnd }
    <0> "for"           { make TokenFor }
    <0> "from"          { make TokenFrom }
    <0> "to"            { make TokenTo }
    <0> "if"            { make TokenIf }
    <0> "otherwise"     { make TokenOtherwise }
    <0> "while"         { make TokenWhile }

    -- Functions and Procedures
    <0> "finish"        { make TokenFinish }
    <0> "function"      { make TokenFunction }
    <0> "procedure"     { make TokenProcedure }
    <0> "return"        { make TokenReturn }
    <0> ":-"            { make TokenDefine }

    -- Composite Types
    <0> "either"        { make TokenEither }
    <0> "record"        { make TokenRecord }

    -- Conversion
    <0> "toBoolean"     { make TokenToBoolean }
    <0> "toCharacter"   { make TokenToCharacter }
    <0> "toFloat"       { make TokenToFloat }
    <0> "toInteger"     { make TokenToInteger }

    -- Types
    <0> "boolean"       { make TokenBooleanType }
    <0> "character"     { make TokenCharacterType }
    <0> "float"         { make TokenFloatType }
    <0> "integer"       { make TokenIntegerType }
    <0> "string"        { make TokenStringType }
    <0> "void"          { make TokenVoidType }

    -- Punctuation
    <0> ","             { make TokenComma }
    <0> "."             { make TokenPeriod }
    <0> ";"             { make TokenSemicolon }
    <0> "->"            { make TokenArrow }
    <0> "("             { make TokenLeftParenthesis }
    <0> ")"             { make TokenRightParenthesis }
    <0> "_"             { make TokenUnderscore }

    -- Assignment
    <0> "is"            { make TokenIs }

    -- IO
    <0> "read"          { make TokenRead }
    <0> "write"         { make TokenWrite }

    -- Literals
    ---- Chars
    <0> @char           { make' $ TokenCharacterLiteral . read }

    ---- Floats
    <0> @float          { make' floatLiteral }

    ---- Ints
    <0> @decimal
     |  @octal
     |  @hexadecimal    { make' integerLiteral }

    ---- Bools
    <0> "true"          { make $ TokenBooleanLiteral True }
    <0> "false"         { make $ TokenBooleanLiteral False }

    ---- Strings
    <0> @string         { make' $ TokenStringLiteral . read }
    <0> @badstring      { make' $ ErrorUnclosedStringLiteral . tail }

    -- Identifier
    <0> @varid          { make' $ TokenVariableIdentifier . id }
    <0> @genid          { make' $ TokenGeneralIdentifier . id }

    -- Unexpected Token
    <0> .               { make' $ ErrorUnexpectedToken . head }

{ ------------------------------------------------------------------------------

type Action = AlexInput -> Int -> Alex (Lexeme Token)

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ r c) = Position (r, c)

make' :: (String -> Token) -> Action
make' t (p, _, _, str) size =
    return $ Lexeme (toPosition p) (t $ take size str)

make :: Token -> Action
make  = make' . const

floatLiteral :: String -> Token
floatLiteral str = if 
    | value < (minValue :: Float) -> ErrorUnderflow str
    | value < (minValue :: Float) -> ErrorUnderflow str
    | otherwise -> TokenFloatLiteral value
    where value = read str :: Float



integerLiteral :: String -> Token
integerLiteral str = do
    let value = read str :: Integer
    if  | value < fromIntegral (minBound :: Int32) -> ErrorUnderflow str
        | value > fromIntegral (maxBound :: Int32) -> ErrorOverflow str
        | otherwise -> TokenIntegerLiteral . fromIntegral $ value

{- Esta función no será necesaria con el Parser -}
alexEOF :: Alex (Lexeme Token)
alexEOF = liftM (\x -> Lexeme x EOF) alexGetPosition

{- Esta función no será necesaria con el Parser -}
alexGetPosition :: Alex Position
alexGetPosition = alexGetInput >>= \(p,_,_,_) -> return $ toPosition p

-- states
state_initial :: Int
state_initial = 0

-- actions
---- comments
enterNewComment :: Action
enterNewComment input len = do
        setLexerCommentDepth 1
        skip input len
embedComment :: Action
embedComment input len = do
        cd <- getLexerCommentDepth
        setLexerCommentDepth (cd + 1)
        skip input len
unembedComment :: Action
unembedComment input len = do
        cd <- getLexerCommentDepth
        setLexerCommentDepth (cd - 1)
        when (cd == 1) (alexSetStartCode state_initial)
        skip input len

-- The user state monad
data AlexUserState = AlexUserState
    { --errors            :: Seq Error
    --,
        lexerCommentDepth :: Int
    }

alexInitUserState :: AlexUserState
alexInitUserState =
    AlexUserState
        { --errors            = Seq.empty
        --,
            lexerCommentDepth = 0
        }

getFromUserState :: (AlexUserState -> a) -> Alex a
getFromUserState f =
    Alex $ \s@AlexState{alex_ust=ust} -> Right (s, f ust)
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f =
    Alex $ \s -> let st = alex_ust s in Right (s {alex_ust = f st}, ())

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = getFromUserState lexerCommentDepth
setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth d = modifyUserState $ \st -> st { lexerCommentDepth = d }

{- Esta función no será necesaria con el Parser -}
scanner :: String -> Either String [Lexeme Token]
scanner str =
    let loop = do
            (t, m) <- alexComplementError alexMonadScan
            when (isJust m) (lexerError (fromJust m))
            let tok@(Lexeme p cl) = t
            if (cl == EOF)
                then return []
                else do
                    toks <- loop
                    return (tok : toks)
    in runAlex str loop

{- Esta función no será necesaria con el Parser -}
lexerError :: String -> Alex a
lexerError msg =
    do
        (p, c, _, inp) <- alexGetInput
        let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
        let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
        let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
        let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
        alexError (disp3 ++ " at " ++ show p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

{- Esta función no será necesaria con el Parser -}
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) =
    Alex
        (\s -> case al s of
            Right (s', x) -> Right (s', (x, Nothing))
            Left  message -> Right (s, (undefined, Just message)))


runAlex' :: String -> Alex a -> (a, String)
runAlex' input (Alex f) =
    let Right (st, a) = f state
        ust = ""
    in (a, ust)
    where
        state :: AlexState
        state = AlexState
            { alex_pos   = alexStartPos
            , alex_inp   = input
            , alex_chr   = '\n'
            , alex_bytes = []
            , alex_ust   = alexInitUserState
            , alex_scd   = 0
            }
}
