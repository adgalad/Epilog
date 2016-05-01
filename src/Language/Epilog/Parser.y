{
module Language.Epilog.Parser
    ( parseProgram
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer
import           Language.Epilog.Expression
import           Language.Epilog.Instruction
import           Data.Int                (Int32)

import           Data.Sequence           (Seq, empty, fromList, index,
                                          singleton, (><), (|>), (<|))
--------------------------------------------------------------------------------
}

%name parser
%tokentype { Lexeme Token }
%monad { Alex }
%lexer { lexer } { Lexeme _ EOF }
%error { parseError }


-- Tokens
%token
    -- Arithmetic
    "+"         { Lexeme _ TokenPlus            }
    "-"         { Lexeme _ TokenMinus           }
    "*"         { Lexeme _ TokenTimes           }
    "/"         { Lexeme _ TokenFloatDivision   }
    "rem"       { Lexeme _ TokenRem             }
    "div"       { Lexeme _ TokenIntegerDivision }

    -- Logical
    "and"       { Lexeme _ TokenAnd     }
    "andalso"   { Lexeme _ TokenAndalso }
    "or"        { Lexeme _ TokenOr      }
    "orelse"    { Lexeme _ TokenOrelse  }
    "not"       { Lexeme _ TokenNot     }

    -- Bitwise
    "band"      { Lexeme _ TokenBand }
    "bor"       { Lexeme _ TokenBor  }
    "bxor"      { Lexeme _ TokenBxor }
    "bnot"      { Lexeme _ TokenBnot }

    -- Relational
    "<"         { Lexeme _ TokenLT       }
    "=<"        { Lexeme _ TokenLTE      }
    ">"         { Lexeme _ TokenGT       }
    ">="        { Lexeme _ TokenGTE      }
    "|"         { Lexeme _ TokenFactorOf }

    -- Equality
    "="         { Lexeme _ TokenEQ }
    "/="        { Lexeme _ TokenNE }

    -- Punctuation
    "->"        { Lexeme _ TokenArrow }
    ","         { Lexeme _ TokenComma }
    ";"         { Lexeme _ TokenSemicolon }
    "."         { Lexeme _ TokenPeriod }
    "is"        { Lexeme _ TokenIs }
    "("         { Lexeme _ TokenLeftParenthesis }
    ")"         { Lexeme _ TokenRightParenthesis }

    -- Control Structures
    "end"       { Lexeme _ TokenEnd       }
    "for"       { Lexeme _ TokenFor       }
    "from"      { Lexeme _ TokenFrom      }
    "to"        { Lexeme _ TokenTo        }
    "if"        { Lexeme _ TokenIf        }
    "otherwise" { Lexeme _ TokenOtherwise }
    "while"     { Lexeme _ TokenWhile     }

    -- Functions and Procedures
    "finish"    { Lexeme _ TokenFinish    }
    "function"  { Lexeme _ TokenFunction  }
    "procedure" { Lexeme _ TokenProcedure }
    "return"    { Lexeme _ TokenReturn    }
    ":-"        { Lexeme _ TokenDefine    }


    "true"      { Lexeme _ ( TokenBooleanLiteral   _ ) }
    "false"     { Lexeme _ ( TokenBooleanLiteral   _ ) }
    char        { Lexeme _ ( TokenCharacterLiteral _ ) }
    float       { Lexeme _ ( TokenFloatLiteral     _ ) }
    int         { Lexeme _ ( TokenIntegerLiteral   _ ) }
    string      { Lexeme _ ( TokenStringLiteral    _ ) }

    varid       { Lexeme _ ( TokenVariableIdentifier _ ) }
    genid       { Lexeme _ ( TokenGeneralIdentifier  _ ) }

    boolType    { Lexeme _ TokenBooleanType   }
    charType    { Lexeme _ TokenCharacterType }
    floatType   { Lexeme _ TokenFloatType     }
    intType     { Lexeme _ TokenIntegerType   }
    strType     { Lexeme _ TokenStringType    }

-- Precedence

-- -- Bitwise
%left     "bor"
%left     "bxor"
%left     "band"
%right    "bnot"

-- -- Logical
%left     "or" "orelse"
%left     "and" "andalso"
%right    "not"

-- -- -- Compare
%left     "=" "/="
%nonassoc "<" "=<" ">" ">="

-- -- Arithmetic
%left     "+" "-"
%left     "*" "/" "rem" "div"
%right    "-"
%nonassoc "|"

-- -- Assign
%right    "is"

%% -----------------------------------------------------------------------------
-- Grammar

-- Instruction 
ProcDef :: { Lexeme Method }
    : "procedure" GenId "(" ParamList ")" ":-" InstList "."  
        { Proc $2 $4 $7 <$ $1 }
    | "function" GenId "(" ParamList ")" "->" Type ":-" InstList "."
        { Func $2 $4 $7 $9 <$ $1 }


ParamList :: { InstBlock }
    : Type VarId                { fromList [Declaration $1 $2 <$ $1] }
    | ParamList "," Type VarId  { $1 |> (Declaration $3 $4 <$ $3) }

InstList :: { InstBlock }
    : Instruction               { $1 }
    | InstList "," Instruction  { $1 >< $3 }

Instruction :: { InstBlock }
    :{- λ -}                { empty }
    | Assign                { fromList [$1] }
    | Declaration           { fromList [$1] }
    | "if" GuardList "end"  { fromList [If $2 <$ $1] }
    | "return" Expression   { fromList [Return $2 <$ $1] }


Assign :: { Lexeme Instruction }
    : VarId "is" Expression   { Assign (VarId $1 <$ $1) $3 <$ $1 }

Declaration :: { Lexeme Instruction }
    : Type VarId  { Declaration $1 $2 <$ $1 }
    | Type Assign { Initialization $1 $2 <$ $1}

GuardList :: { InstBlock }
    : Expression "->" InstList               { fromList [Guard $1 $3 <$ $1]}
    | GuardList ";" Expression "->" InstList { $1 |> (Guard $3 $5 <$ $3) }






Type :: { Lexeme Type }
    : boolType  { BoolT <$ $1 }
    | charType  { CharT <$ $1 }
    | floatType { FloatT <$ $1 }
    | intType   { IntT <$ $1 }
    | strType   { StringT <$ $1 }

-- Expressions
Expression :: { Lexeme Expression }
    : Char      { LitChar $1 <$ $1}
    | Bool      { LitBool $1 <$ $1}
    | Float     { LitFloat $1 <$ $1}
    | Int       { LitInt $1 <$ $1}
    | String    { LitString $1 <$ $1 }
    | VarId     { VarId $1 <$ $1 }
    | GenId     { GenId $1 <$ $1 }
    -- | "(" Expression ")"          { }

    -- -- Arithmetic
    | Expression "+" Expression  { BinaryExp (Plus <$ $2) $1 $3 <$ $1 }
    | Expression "-" Expression  { BinaryExp (Minus <$ $2) $1 $3 <$ $1 }
    | Expression "*" Expression  { BinaryExp (Times <$ $2) $1 $3 <$ $1 }
    | Expression "/" Expression  { BinaryExp (FloatDivision <$ $2) $1 $3 <$ $1 }
    | Expression "div" Expression{ BinaryExp (IntegerDivision <$ $2) $1 $3 <$ $1 }
    | Expression "rem" Expression{ BinaryExp (Rem <$ $2) $1 $3 <$ $1 }

    -- -- Logical
    | Expression "and"     Expression { BinaryExp (And     <$ $2) $1 $3 <$ $1 }
    | Expression "or"      Expression { BinaryExp (Or      <$ $2) $1 $3 <$ $1 }
    | Expression "orelse"  Expression { BinaryExp (Orelse  <$ $2) $1 $3 <$ $1 }
    | Expression "andalso" Expression { BinaryExp (Andalso <$ $2) $1 $3 <$ $1 }
    |            "not"     Expression { UnaryExp  (Not     <$ $1) $2 <$ $1 }


    -- -- Bitwise
    | Expression "band" Expression { BinaryExp (Band <$ $2) $1 $3 <$ $1 }
    | Expression "bor"  Expression { BinaryExp (Bor  <$ $2) $1 $3 <$ $1 }
    | Expression "bxor" Expression { BinaryExp (Bxor <$ $2) $1 $3 <$ $1 }
    |            "bnot" Expression { UnaryExp  (Bnot <$ $1) $2 <$ $1 }

    -- -- Comparison
    | Expression "<" Expression    { BinaryExp (LTop <$ $2) $1 $3 <$ $1 }
    | Expression "=<" Expression   { BinaryExp (LTEop <$ $2) $1 $3 <$ $1 }
    | Expression ">" Expression    { BinaryExp (GTop <$ $2) $1 $3 <$ $1 }
    | Expression ">=" Expression   { BinaryExp (GTEop <$ $2) $1 $3 <$ $1 }
    | Expression "|" Expression    { BinaryExp (FactorOf <$ $2) $1 $3 <$ $1 }

    -- -- Relational
    | Expression "=" Expression    { BinaryExp (Equal <$ $2) $1 $3 <$ $1 }
    | Expression "/=" Expression   { BinaryExp (NoEqual <$ $2) $1 $3 <$ $1 }

Bool   :: { Lexeme Bool }
    : "true"   { unTokenBooleanLiteral `fmap` $1 }
    | "false"  { unTokenBooleanLiteral `fmap` $1 }

Char   :: { Lexeme Char }
    : char   { unTokenCharacterLiteral `fmap` $1 }

Float  :: { Lexeme Float }
    : float  { unTokenFloatLiteral   `fmap` $1 }

Int    :: { Lexeme Int32 }
    : int    { unTokenIntegerLiteral `fmap` $1 }

String :: { Lexeme String }
    : string { unTokenStringLiteral  `fmap` $1 }

VarId :: { Lexeme String }
    : varid { unTokenVariableIdentifier `fmap` $1 }

GenId :: { Lexeme String }
    : genid { unTokenGeneralIdentifier  `fmap` $1 }




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