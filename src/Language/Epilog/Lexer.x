{ {-# OPTIONS_GHC -w #-}
  {-# LANGUAGE MultiWayIf #-}
module Language.Epilog.Lexer
    ( Token (..)
    , lexer
    , isError
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.Token
--------------------------------------------------------------------------------
import           Control.Monad  (liftM, when)
import qualified Data.Bits
import           Data.Char      (ord)
import           Data.Int       (Int32)
import           Data.Maybe     (fromJust, isJust)
import           Numeric.Limits (maxValue, minValue)
import           Control.Lens   ((^.), (.=), (+=), (-=), (<-=), _1, use)
--------------------------------------------------------------------------------
}
-- We don't use any wrapper because we want to use our own Monad

$octit       = [0-7]
$digit       = [0-9]
$hexit       = [0-9 A-F a-f]

@octal       = 0[oO] $octit+
@decimal     = $digit+
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
-- \\"'
@escape      = \\ ($charesc)

@charval     = ($graphic | @escape)
@stringval   = @charval*

@char        = \' @charval \'
@string      = \" @stringval \"
-- \\"
@badstring   = \" @stringval
-- \\"

--------------------------------------------------------------------------------
epilog :-

    -- Whitespace
        $white+                 ;

    -- Comments
    <0> \% \% .*                ;
    <0> "/%"                    { changeCommentDepth (.=1) }
    <c> "/%"                    { changeCommentDepth (+=1) }
    <c> "%/"                    { changeCommentDepth (-=1) }
    <c> .                       ;

    -- Operators
    ---- Logical
    <0> "and"                   { make TokenAnd     }
    <0> "andalso"               { make TokenAndalso }
    <0> "or"                    { make TokenOr      }
    <0> "orelse"                { make TokenOrelse  }
    <0> "xor"                   { make TokenXor  }
    <0> "not"                   { make TokenNot     }

    ---- Bitwise
    <0> "band"                  { make TokenBand }
    <0> "bor"                   { make TokenBor  }
    <0> "bnot"                  { make TokenBnot }
    <0> "bsl"                   { make TokenBsl  }
    <0> "bsr"                   { make TokenBsr  }
    <0> "bxor"                  { make TokenBxor }

    ---- Array / Record / Either
    <0> "length"                { make TokenLength       }
    <0> "_"                     { make TokenUnderscore   }
    <0> "["                     { make TokenLeftBracket  }
    <0> "]"                     { make TokenRightBracket }
    <0> "{"                     { make TokenLeftBrace    }
    <0> "}"                     { make TokenRightBrace   }

    ---- Arithmetic
    <0> "+"                     { make TokenPlus     }
    <0> "-"                     { make TokenMinus    }
    <0> "*"                     { make TokenTimes    }
    <0> "/"                     { make TokenFloatDiv }
    <0> "div"                   { make TokenIntDiv   }
    <0> "rem"                   { make TokenRem      }

    ---- Relational
    <0> "<"                     { make TokenLT }
    <0> "=<"                    { make TokenLE }
    <0> ">"                     { make TokenGT }
    <0> ">="                    { make TokenGE }
    <0> "="                     { make TokenEQ }
    <0> "/="                    { make TokenNE }
    <0> "|"                     { make TokenFA }
    <0> "!|"                    { make TokenNF }

    -- Control Structures
    <0> "end"                   { make TokenEnd       }
    <0> "for"                   { make TokenFor       }
    <0> "from"                  { make TokenFrom      }
    <0> "to"                    { make TokenTo        }
    <0> "if"                    { make TokenIf        }
    <0> "otherwise"             { make TokenOtherwise }
    <0> "while"                 { make TokenWhile     }
    <0> "case"                  { make TokenCase      }
    <0> "of"                    { make TokenOf        }

    -- Procedures
    <0> "procedure"             { make TokenProcedure }
    <0> ":-"                    { make TokenDefine    }
    <0> "finish"                { make TokenFinish    }

    -- Composite Types
    <0> "either"                { make TokenEither }
    <0> "record"                { make TokenRecord }

    -- Global Declaration
    <0> "global"                { make TokenGlobal }

    -- Punctuation
    <0> ","                     { make TokenComma     }
    <0> "."                     { make TokenPeriod    }
    <0> ";"                     { make TokenSemicolon }
    <0> "->"                    { make TokenArrow     }
    <0> "("                     { make TokenLeftPar   }
    <0> ")"                     { make TokenRightPar  }

    -- Assignment
    <0> "is"                    { make TokenIs  }

    -- IO
    <0> "read"                  { make TokenRead  }
    <0> "write"                 { make TokenWrite }

    -- Literals
    ---- Bools
    <0> "true"                  { make $ TokenBoolLit True  }
    <0> "false"                 { make $ TokenBoolLit False }

    ---- Chars
    <0> @char                   { make' $ TokenCharLit . read }

    ---- Floats
    <0> 0 \. 0+ @exponent ?     { make $ TokenFloatLit 0.0 }
    <0> @float                  { make' floatLiteral       }

    ---- Ints
    <0> @decimal
     |  @octal
     |  @hexadecimal            { make' integerLiteral }

    ---- Strings
    <0> @string                 { make' $ TokenStringLit . read         }
    <0> @badstring              { make' $ ErrorUnclosedStringLit . tail }

    -- Identifier
    <0> @varid                  { make' $ TokenVarId . id }
    <0> @genid                  { make' $ TokenGenId . id }

    -- Unexpected Token
    <0> .                       { make' $ ErrorUnexpectedToken . head }

{ ------------------------------------------------------------------------------
-- Bytes -------------------------------
-- | Converts a Char into the corresponding list of Bytes
utf8Encode :: Char -> [Byte]
utf8Encode = map fromIntegral . go . ord
    where
        go oc
            | oc <= 0x7f   =
                [oc]
            | oc <= 0x7ff  =
                [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                , 0x80 + oc Data.Bits..&. 0x3f
                ]
            | oc <= 0xffff =
                [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                , 0x80 + oc Data.Bits..&. 0x3f
                ]
            | otherwise    =
                [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                , 0x80 + oc Data.Bits..&. 0x3f
                ]

-- | Gets the next Byte
alexGetByte :: LexerInput -> Maybe (Byte, LexerInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[])    = Nothing
alexGetByte (p,_,[],(c:s)) =
    let p' = alexMove p c
        (b:bs) = utf8Encode c
    in p' `seq` Just (b, (p', c, bs, s))

-- | Ignores remaining bytes in current char
ignorePendingBytes :: LexerInput -> LexerInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)


-- Lexer input -------------------------
-- | Alex wants this type
type LexerInput =
    ( Position -- current position,
    , Char     -- previous char
    , [Byte]   -- pending bytes on current char
    , String   -- current input string
    )
type AlexInput = LexerInput -- Alex

getInput :: Epilog LexerInput
getInput = do
    s <- get
    return (s^.position, s^.prevChar, s^.bytes, s^.input)


-- Utility functions -------------------
-- | Updates the position after moving one char
alexMove :: Position -> Char -> Position
alexMove (Position (l, c)) '\t' =
    Position (l, (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1))
alexMove (Position (l, c)) '\n' =
    Position ((l+1), 1)
alexMove (Position (l, c)) _    =
    Position (l, (c+1))

skip _input _len = readToken

initialScanCode :: Int
initialScanCode = 0

changeCommentDepth change input len = do
    change commentDepth
    cd <- use commentDepth
    scanCode .= if (cd == 0)
        then initialScanCode
        else c
    skip input len


-- Token builders ----------------------
-- | For tokens which need their value
make' :: (String -> Token) -> LexerInput -> Int -> Epilog (At Token)
make' t (p, _, _, str) size =
    return $ (t $ take size str) :@ p

-- | For tokens without a value
make :: Token -> LexerInput -> Int -> Epilog (At Token)
make = make' . const

floatLiteral :: String -> Token
floatLiteral str = if
    | value > realToFrac (maxValue :: Float) -> ErrorOverflow str
    | value < realToFrac (minValue :: Float) -> ErrorUnderflow str
    | otherwise -> TokenFloatLit (read str :: Float)
    where value = read str :: Double

integerLiteral :: String -> Token
integerLiteral str = if
    | value > fromIntegral (maxBound :: Int32) -> ErrorOverflow str
    | otherwise -> TokenIntLit . fromIntegral $ value
    where value = read str :: Integer


-- The Lexer itself --------------------
readToken = do
    inp <- getInput
    code <- use scanCode
    case alexScan inp code of
        AlexEOF -> return $
            EOF :@ (inp^._1)
        AlexError (p,_,_,_) -> do
            err $ LexicalError p
            readToken
        AlexSkip  (pos, c, bs, inp') len -> do
            position .= pos
            prevChar .= c
            bytes    .= bs
            input    .= inp'
            readToken
        AlexToken (pos, c, bs, inp') len action -> do
            position .= pos
            prevChar .= c
            bytes    .= bs
            input    .= inp'
            action (ignorePendingBytes inp) len

lexer :: (At Token -> Epilog a) -> Epilog a
lexer cont = readToken >>= cont
}
