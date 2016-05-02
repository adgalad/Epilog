{ {-# OPTIONS_GHC -w #-}
  {-# LANGUAGE MultiWayIf #-}
module Language.Epilog.Lexer
    ( Alex (..)
    , At (..)
    , Token (..)
    , alexMonadScan
    , isError
    , runAlex'
    , scanner
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Token

import           Numeric.Limits         (minValue, maxValue)
import           Data.Int               (Int32)
import           Control.Monad          (liftM, when)
import           Data.Maybe             (fromJust, isJust)
--------------------------------------------------------------------------------
}

%wrapper "monadUserState"

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
@escape      = \\ ($charesc)

@charval     = ($graphic | @escape)
@stringval   = @charval*

@char        = \' @charval \'
@string      = \" @stringval \"
@badstring   = \" @stringval

--------------------------------------------------------------------------------
epilog :-

    -- Whitespace
    <0> $white+                 ;

    -- Comments
    <0> "%%".*                  ;
    <0> "/%"                    { enterNewComment `andBegin` c }
    <c> "/%"                    { embedComment   }
    <c> "%/"                    { unembedComment }
    <c> .                       ;
    <c> "\n"                    { skip }

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
    <0> "length"                { make TokenLength     }
    <0> ":"                     { make TokenColon      }
    <0> "_"                     { make TokenUnderscore }

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

    -- Functions and Procedures
    <0> "finish"                { make TokenFinish    }
    <0> "function"              { make TokenFunction  }
    <0> "procedure"             { make TokenProcedure }
    <0> "return"                { make TokenReturn    }
    <0> ":-"                    { make TokenDefine    }

    -- Composite Types
    <0> "either"                { make TokenEither }
    <0> "record"                { make TokenRecord }

    -- Global Declaration
    <0> "global"                { make TokenGlobal }

    -- Conversion
    <0> "toBoolean"             { make TokenToBool  }
    <0> "toCharacter"           { make TokenToChar  }
    <0> "toInteger"             { make TokenToInt   }
    <0> "toFloat"               { make TokenToFloat }

    -- Types
    <0> "void"                  { make TokenVoidType   }
    <0> "boolean"               { make TokenBoolType   }
    <0> "character"             { make TokenCharType   }
    <0> "integer"               { make TokenIntType    }
    <0> "float"                 { make TokenFloatType  }
    <0> "string"                { make TokenStringType }

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

type Action = AlexInput -> Int -> Alex (At Token)

toPair :: AlexPosn -> (Int, Int)
toPair (AlexPn _ r c) = (r, c)

make' :: (String -> Token) -> Action
make' t (p, _, _, str) size =
    return $ (t $ take size str) :@ (toPair p)

make :: Token -> Action
make  = make' . const

floatLiteral :: String -> Token
floatLiteral str = if
    | value > ( maxValue :: Float) -> ErrorOverflow str
    | otherwise -> TokenFloatLit value
    where value = read str :: Float

integerLiteral :: String -> Token
integerLiteral str = if
    | value > fromIntegral (maxBound :: Int32) -> ErrorOverflow str
    | otherwise -> TokenIntLit . fromIntegral $ value
    where value = read str :: Integer

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
data AlexUserState = AlexUserState { lexerCommentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0 }

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

-- The basic lexer
alexEOF :: Alex (At Token)
alexEOF = (EOF :@) . toPair . fst4 <$> alexGetInput
    where
        fst4 (x,_,_,_) = x

scanner :: String -> Either String [At Token]
scanner str =
    let loop = do
            t <- alexMonadScan
            let tok@(i :@ (r, c)) = t
            if (i == EOF)
                then return []
                else do
                    toks <- loop
                    return (tok : toks)
    in runAlex str loop

-- The lexer for happy
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
