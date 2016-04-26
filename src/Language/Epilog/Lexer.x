{ {-# OPTIONS_GHC -w #-}

module Language.Epilog.Lexer
    ( Alex (..)
    , Token (..)
    , Lexeme (..)
    , alexMonadScan
    , scanner
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Error
import           Language.Epilog.Lexeme
import           Language.Epilog.Token

import           Control.Monad          (liftM, when)
import           Data.Maybe             (fromJust, isJust)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq (empty, (|>))
--------------------------------------------------------------------------------
}

%wrapper "monadUserState"

$digit = 0-9
$upper = [A-Z]
$lower = [a-z]
$alpha = [$upper $lower]




@exponent       = [eE] [\-\+]? $digit+
@float = $digit+ \. $digit+ @exponent? | $digit+ @exponent
@hexit          = 0[xX][$digit A-F a-f]+

@id        = [A-Z][$alpha $digit \’]*
@badid     = $alpha[$alpha $digit \’ ]*

@char      = \'.?\'
@string    = \".*\"
@boolean   = "true" | "false"


--------------------------------------------------------------------------------
tokens :-

    -- Whitespace
    $white+         ;

    -- Comments
        "%%".*      ;
    <0> "/%"        { enterNewComment `andBegin` c }
    <c> "/%"        { embedComment }
    <c> "%/"        { unembedComment }
    <c> .           ;
    <c> "\n"        { skip }

    -- Language Keywords

    <0> "either"    { make' TokenEither }
    <0> "end"       { make' TokenEnd    }
    <0> "finish"    { make' TokenFinish }
    <0> "for"       { make' TokenFor    }
    <0> "if"        { make' TokenIf     }
    <0> "is"        { make' TokenIs     } 
    <0> "otherwise" { make' TokenOtherwise  }
    <0> "record"    { make' TokenRecord }
    <0> "return"    { make' TokenReturn }
    <0> "while"     { make' TokenWhile  }
    <0> "read"      { make' TokenRead  }
    <0> "print"     { make' TokenPrint  }
        -- Conversion
        <0> "toBolean"  { make' TokenToBoolean   }
        <0> "toCharacter" { make' TokenToCharacter }
        <0> "toFloat"   { make' TokenToFloat     }
        <0> "toInteger" { make' TokenToInteger   }

        -- Types
        <0> "boolean"   { make' TokenBooleanType }
        <0> "character" { make' TokenCharacterType }
        <0> "float"     { make' TokenFloatType   }
        <0> "integer"   { make' TokenIntegerType }
        <0> "string"    { make' TokenStringType  }
        <0> "void"      { make' TokenVoidType    }

    -- Language Punctuation

    <0> ","         { make' TokenComma            }
    <0> "."         { make' TokenDot              }
    <0> ";"         { make' TokenSemiColon        }
    <0> ":"         { make' TokenColon            }
    <0> "->"        { make' TokenArrow            }
    <0> "("         { make' TokenOpenParenthesis  }
    <0> ")"         { make' TokenCloseParenthesis }
    <0> "{"         { make' TokenOpenCurly        }
    <0> "}"         { make' TokenCloseCurly       }
    <0> "_"         { make' TokenUnderscore       }

    -- Relational
    <0> "=<"        { make' TokenLessThanOrEqual     }
    <0> "<"         { make' TokenLessThan            }
    <0> ">="        { make' TokenGreaterThanOrEqual  }
    <0> ">"         { make' TokenGreaterThan         }
    <0> "|"         { make' TokenFactorOf            }

    -- Equality
    <0> "="         { make' TokenEqualTo     }
    <0> "/="        { make' TokenNotEqualTo  }

    -- Bitwise Operators
    <0> "band"      { make' TokenBand }
    <0> "bnot"      { make' TokenBnot }
    <0> "bor"       { make' TokenBor  }
    <0> "bsl"       { make' TokenBsl  }
    <0> "bsr"       { make' TokenBsr  }
    <0> "bxor"      { make' TokenBxor }

    -- Arithmetic Operators
    <0> "+"         { make' TokenPlus   }
    <0> "-"         { make' TokenMinus  }
    <0> "*"         { make' TokenTimes  }
    <0> "/"         { make' TokenDivision }
    <0> "div"       { make' TokenIntegerDivision }
    <0> "rem"       { make' TokenRem    }

    -- Logical Operators
    <0> "andalso"   { make' TokenAndalso }
    <0> "and"       { make' TokenAnd        }
    <0> "orelse"    { make' TokenOrelse  }
    <0> "or"        { make' TokenOr      }
    <0> "not"       { make' TokenNot     }

    -- Literals 
    <0> @boolean    { make $ TokenBoolean   . (\x -> if x == "true" then True else False) }
    <0> @char       { make $ TokenCharacter . read }
    <0> $digit+     { make $ TokenInteger   . read }
    <0> @float      { make $ TokenFloat     . read }
    <0> @hexit      { make $ TokenInteger   . read }
    <0> @string     { make $ TokenString    . init . tail   }
    

    -- Identifiers
    <0> @id         { make $ TokenVariableIdentifier . id }
    <0> @badid      { make $ TokenErrorIdentifier . id}




{
--------------------------------------------------------------------------------

type Action = AlexInput -> Int -> Alex (Lexeme Token)

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ r c) = Position (r, c)

alexEOF :: Alex (Lexeme Token)
alexEOF = liftM (\x -> Lexeme x TokenEOF) alexGetPosition

alexGetPosition :: Alex Position
alexGetPosition = alexGetInput >>= \(p,_,_,_) -> return $ toPosition p


make :: (String -> Token) -> Action
make t (p, _, _, str) size = return $ Lexeme (toPosition p) (t $ take size str)

make' :: Token -> Action
make' = make . const

-- states
state_initial :: Int
state_initial = 0

-- actions
enterNewComment, embedComment, unembedComment :: Action
enterNewComment input len = do
        setLexerCommentDepth 1
        skip input len
embedComment input len = do
        cd <- getLexerCommentDepth
        setLexerCommentDepth (cd + 1)
        skip input len
unembedComment input len = do
        cd <- getLexerCommentDepth
        setLexerCommentDepth (cd - 1)
        when (cd == 1) (alexSetStartCode state_initial)
        skip input len

-- The user state monad
data AlexUserState = AlexUserState
    { errors            :: Seq Error
    , lexerCommentDepth :: Int
    , lexerStringState  :: Bool
    , lexerStringValue  :: String
    }

alexInitUserState :: AlexUserState
alexInitUserState =
    AlexUserState
        { errors            = Seq.empty
        , lexerCommentDepth = 0
        , lexerStringState  = False
        , lexerStringValue  = ""
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

getLexerStringState :: Alex Bool
getLexerStringState = getFromUserState lexerStringState
setLexerStringState :: Bool -> Alex ()
setLexerStringState s = modifyUserState $ \st -> st { lexerStringState = s}

getLexerStringValue :: Alex String
getLexerStringValue = getFromUserState lexerStringValue
setLexerStringValue :: String -> Alex ()
setLexerStringValue v = modifyUserState $ \st -> st { lexerStringValue = v }

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c =
    modifyUserState $ \st -> st {lexerStringValue = c:lexerStringValue st}

scanner :: String -> Either String [Lexeme Token]
scanner str =
    let loop = do
            (t, m) <- alexComplementError alexMonadScan
            when (isJust m) (lexerError (fromJust m))
            let tok@(Lexeme p cl) = t
            if (cl == TokenEOF)
                then do
                    f1 <- getLexerStringState
                    d2 <- getLexerCommentDepth
                    if ((not f1) && (d2 == 0))
                        then return [tok]
                        else if (f1)
                            then alexError "String not closed at end of file"
                            else alexError "Comment not closed at end of file"
                else do
                    toks <- loop
                    return (tok : toks)
    in runAlex str loop

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

alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) =
    Alex
        (\s -> case al s of
            Right (s', x) -> Right (s', (x, Nothing))
            Left  message -> Right (s, (undefined, Just message)))
}
