{
module Language.Epilog.Parser
    ( parseProgram
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer
import           Language.Epilog.Expression
import           Data.Int                (Int32)
--------------------------------------------------------------------------------
}

%name parser
%tokentype { Lexeme Token }
%monad { Alex }
%lexer { lexer } { Lexeme _ EOF }
%error { parseError }


-- Tokens
%token

    "+"         { Lexeme _ TokenPlus            }
    "-"         { Lexeme _ TokenMinus           }
    "*"         { Lexeme _ TokenTimes           }
    "/"         { Lexeme _ TokenFloatDivision   }
    "rem"       { Lexeme _ TokenRem             }
    "div"       { Lexeme _ TokenIntegerDivision }

    char        { Lexeme _ ( TokenCharacterLiteral _ ) }
    float       { Lexeme _ ( TokenFloatLiteral     _ ) }
    int         { Lexeme _ ( TokenIntegerLiteral   _ ) }
    bool        { Lexeme _ ( TokenBooleanLiteral   _ ) }
    string      { Lexeme _ ( TokenStringLiteral    _ ) }

-- Precedence
-- -- Bool
%left     "or"
%left     "and"
%right    "not"

-- -- -- Compare
%left     "=" "/="
%nonassoc "<" "=<" ">" ">="

-- -- Arithmetic
%left     "+" "-"
%left     "*" "/" "rem" "div"
%right    "-"
%nonassoc "|"

%% -----------------------------------------------------------------------------
-- Grammar

-- Expressions
Expression :: { Lexeme Expression }
    : Int       { LitInt $1 <$ $1}
    -- | "(" Expression ")"          { }
    | Expression "+" Expression   { BinaryExp (Plus <$ $2) $1 $3 <$ $1 }
    | Expression "-" Expression   { BinaryExp (Minus <$ $2) $1 $3 <$ $1 }
    | Expression "*" Expression   { BinaryExp (Times <$ $2) $1 $3 <$ $1 }
    | Expression "/" Expression   { BinaryExp (FloatDivision <$ $2) $1 $3 <$ $1 }
    | Expression "div" Expression { BinaryExp (IntegerDivision <$ $2) $1 $3 <$ $1 }
    | Expression "rem" Expression { BinaryExp (IntegerDivision <$ $2) $1 $3 <$ $1 }
Int :: { Lexeme Int32 }
      : int       { unTokenIntegerLiteral `fmap` $1 }

{- λ -} 

{ ------------------------------------------------------------------------------
-- Parser
lexer :: (Lexeme Token -> Alex a) -> Alex a
lexer cont = do
    l@(Lexeme _ t) <- alexMonadScan
    case t of
        ErrorUnderflow _ -> do
            lexer cont
        ErrorOverflow _ -> do
            lexer cont
        ErrorUnclosedStringLiteral s -> do
            cont $ TokenStringLiteral s <$ l
        ErrorUnexpectedToken _ -> do
            lexer cont
        _ -> cont l

parseError :: Lexeme Token -> Alex a
parseError (Lexeme p t) = fail $ show p ++ ": Parse error on Token: " ++ show t ++ "\n"

-- parseProgram :: String -> (Expression, String)
parseProgram input = runAlex' input parser
}