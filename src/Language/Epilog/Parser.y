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

%token
    char        { Lexeme _ ( TokenCharacterLiteral _ ) }
    float       { Lexeme _ ( TokenFloatLiteral     _ ) }
    int         { Lexeme _ ( TokenIntegerLiteral   _ ) }
    bool        { Lexeme _ ( TokenBooleanLiteral   _ ) }
    string      { Lexeme _ ( TokenStringLiteral    _ ) }

%% -----------------------------------------------------------------------------
-- Grammar

-- Expressions
Expression :: { Lexeme Expression }
    : Int       { LitInt $1 <$ $1}

Int :: { Lexeme Int32 }
      : int       { unTokenIntegerLiteral `fmap` $1 }

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