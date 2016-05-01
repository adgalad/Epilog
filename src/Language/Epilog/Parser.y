{
module Language.Epilog.Parser
    ( parseProgram
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Lexer
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Instruction

import           Data.Int                (Int32)
import           Data.Sequence           (Seq, empty, fromList, index,
                                          singleton, (><), (|>), (<|))
--------------------------------------------------------------------------------
}

%name parser
%tokentype { At Token }
%monad { Alex }
%lexer { lexer } { EOF :@ _ }
%error { parseError }

-- Tokens
%token
    -- Operators
    ---- Logical
    "and"           { TokenAnd     :@ _ }
    "andalso"       { TokenAndalso :@ _ }
    "or"            { TokenOr      :@ _ }
    "orelse"        { TokenOrelse  :@ _ }
    "xor"           { TokenXor     :@ _ }
    "not"           { TokenNot     :@ _ }

    ---- Bitwise
    "band"          { TokenBand :@ _ }
    "bor"           { TokenBor  :@ _ }
    "bnot"          { TokenBnot :@ _ }
    "bsl"           { TokenBxor :@ _ }
    "bsr"           { TokenBxor :@ _ }
    "bxor"          { TokenBxor :@ _ }

    ---- Array / Record / Either
    "length"        { TokenLength     :@ _ }
    ":"             { TokenColon      :@ _ }
    "_"             { TokenUnderscore :@ _ }

    ---- Arithmetic
    "+"             { TokenPlus     :@ _ }
    "-"             { TokenMinus    :@ _ }
    "*"             { TokenTimes    :@ _ }
    "/"             { TokenFloatDiv :@ _ }
    "div"           { TokenIntDiv   :@ _ }
    "rem"           { TokenRem      :@ _ }

    ---- Relational
    "<"             { TokenLT        :@ _ }
    "=<"            { TokenLE        :@ _ }
    ">"             { TokenGT        :@ _ }
    ">="            { TokenGE        :@ _ }
    "="             { TokenEQ        :@ _ }
    "/="            { TokenNE        :@ _ }
    "|"             { TokenFactor    :@ _ }
    "!|"            { TokenNotFactor :@ _ }

    -- Control Structures
    "end"           { TokenEnd       :@ _ }
    "for"           { TokenFor       :@ _ }
    "from"          { TokenFrom      :@ _ }
    "to"            { TokenTo        :@ _ }
    "if"            { TokenIf        :@ _ }
    "otherwise"     { TokenOtherwise :@ _ }
    "while"         { TokenWhile     :@ _ }

    -- Functions and Procedures
    "finish"        { TokenFinish    :@ _ }
    func            { TokenFunction  :@ _ }
    proc            { TokenProcedure :@ _ }
    "return"        { TokenReturn    :@ _ }
    ":-"            { TokenDefine    :@ _ }

    -- Composite Types
    "either"        { TokenEither :@ _ }
    "record"        { TokenRecord :@ _ }

    -- Conversion
    "toBoolean"     { TokenToBool  :@ _ }
    "toCharacter"   { TokenToChar  :@ _ }
    "toFloat"       { TokenToFloat :@ _ }
    "toInteger"     { TokenToInt   :@ _ }

    -- Types
    "bool"          { TokenBoolType   :@ _ }
    "char"          { TokenCharType   :@ _ }
    "int"           { TokenIntType    :@ _ }
    "float"         { TokenFloatType  :@ _ }
    "string"        { TokenStringType :@ _ }

    -- Punctuation
    ","             { TokenComma      :@ _ }
    "."             { TokenPeriod     :@ _ }
    ";"             { TokenSemicolon  :@ _ }
    "->"            { TokenArrow      :@ _ }
    "("             { TokenLeftPar    :@ _ }
    ")"             { TokenRightPar   :@ _ }

    -- Assignment
    "is"            { TokenIs :@ _ }

    -- IO
    "read"          { TokenRead  :@ _ }
    "write"         { TokenWrite :@ _ }

    -- Literals
    boolLit         { ( TokenBoolLit   _ ) :@ _ }
    charLit         { ( TokenCharLit   _ ) :@ _ }
    intLit          { ( TokenIntLit    _ ) :@ _ }
    floatLit        { ( TokenFloatLit  _ ) :@ _ }
    stringLit       { ( TokenStringLit _ ) :@ _ }

    -- Identifier
    varId           { ( TokenVarId _ ) :@ _ }
    genId           { ( TokenGenId _ ) :@ _ }


-- Precedence
%right    "is"

%left     "orelse"
%left     "andalso"
%left     "or"
%left     "xor"
%left     "and"
%left     "bor"
%left     "bxor"
%left     "band"

%left     "=" "/="
%nonassoc "|" "!|"
%nonassoc "<" "=<" ">" ">="

%left     "bsl" "bsr"
%left     "+" "-"
%left     "*" "/" "div" "rem"

%left     NEG

%left     "_"
%left     ":"
%nonassoc "length"

%% -----------------------------------------------------------------------------
-- Grammar

Program :: { Program }
    : ProcDefs          { Program $1 }

ProcDefs :: { Declarations }
    : ProcDef           { singleton $1 }
    | ProcDef ProcDefs  { $1 <| $2 }

-- Top Level Declarations
ProcDef :: { At Declaration }
    : proc GenId "(" Params ")" ":-" Insts "."
        { ProcD $2 $4 $7 <$ $1 }
    | func GenId "(" Params ")" "->" Type ":-" Insts "."
        { FunD $2 $4 $7 $9 <$ $1 }
    -- Case for either
    -- Case for record
    -- Case for global

Params :: { InstBlock }
    : Type VarId             { singleton (Declaration $1 $2 <$ $1) }
    | Type VarId "," Params  { (Declaration $1 $2 <$ $1) <| $4 }

Insts :: { InstBlock }
    : Instruction            { singleton $1 }
    | Instruction "," Insts  { $1   <|   $3 }

Instruction :: { At Instruction }
    : Assign                { $1 }
    | Declaration           { $1 }
    | If                    { $1 }
    | "finish"              { Finish <$ $1 }
    | "return" Expression   { Return $2 <$ $1 }

Assign :: { At Instruction }
    : VarId "is" Expression   { Assign (VarId $1 <$ $1) $3 <$ $1 }

Declaration :: { At Instruction }
    : Type VarId  { Declaration $1 $2 <$ $1 }
    | Type Assign { Initialization $1 $2 <$ $1}

If :: { At Instruction }
    : "if" GuardList "end" { If $2 <$ $1 }

GuardList :: { InstBlock }
    : Expression "->" Insts               { fromList [Guard $1 $3 <$ $1]}
    | GuardList ";" Expression "->" Insts { $1 |> (Guard $3 $5 <$ $3) }

Type :: { At Type }
    : "bool"   { BoolT <$ $1 }
    | "char"   { CharT <$ $1 }
    | "float"  { FloatT <$ $1 }
    | "int"    { IntT <$ $1 }
    | "string" { StringT <$ $1 }

-- Expressions
Expression :: { At Expression }
    : "(" Expression ")" { $2 }

    | Char      { LitChar $1 <$ $1}
    | Bool      { LitBool $1 <$ $1}
    | Float     { LitFloat $1 <$ $1}
    | Int       { LitInt $1 <$ $1}
    | String    { LitString $1 <$ $1 }
    | VarId     { VarId $1 <$ $1 }
    | GenId     { GenId $1 <$ $1 }

    ---- Logical
    | Expression "and"     Expression { BinaryExp (And     <$ $2) $1 $3 <$ $1 }
    | Expression "andalso" Expression { BinaryExp (Andalso <$ $2) $1 $3 <$ $1 }
    | Expression "or"      Expression { BinaryExp (Or      <$ $2) $1 $3 <$ $1 }
    | Expression "orelse"  Expression { BinaryExp (Orelse  <$ $2) $1 $3 <$ $1 }
    | Expression "xor"     Expression { BinaryExp (Xor     <$ $2) $1 $3 <$ $1 }
    | "not" Expression %prec NEG      { UnaryExp  (Not     <$ $1)    $2 <$ $1 }

    ---- Bitwise
    | Expression "band" Expression { BinaryExp (Band <$ $2) $1 $3 <$ $1 }
    | Expression "bor"  Expression { BinaryExp (Bor  <$ $2) $1 $3 <$ $1 }
    | Expression "bsl"  Expression { BinaryExp (Bsl  <$ $2) $1 $3 <$ $1 }
    | Expression "bsr"  Expression { BinaryExp (Bsr  <$ $2) $1 $3 <$ $1 }
    | Expression "bxor" Expression { BinaryExp (Bxor <$ $2) $1 $3 <$ $1 }
    | "bnot" Expression %prec NEG  { UnaryExp  (Bnot <$ $1) $2 <$ $1 }

    ---- Array / Record / Either
    | Expression ":"  Expression  { BinaryExp (Colon      <$ $2) $1 $3 <$ $1 }
    | Expression "_"  Expression  { BinaryExp (Underscore <$ $2) $1 $3 <$ $1 }
    | "length" Expression         { UnaryExp  (Length <$ $2) $2 <$ $1 }

    ---- Arithmetic
    | Expression "+"   Expression { BinaryExp (Plus     <$ $2) $1 $3 <$ $1 }
    | Expression "-"   Expression { BinaryExp (Minus    <$ $2) $1 $3 <$ $1 }
    | Expression "*"   Expression { BinaryExp (Times    <$ $2) $1 $3 <$ $1 }
    | Expression "/"   Expression { BinaryExp (FloatDiv <$ $2) $1 $3 <$ $1 }
    | Expression "div" Expression { BinaryExp (IntDiv   <$ $2) $1 $3 <$ $1 }
    | Expression "rem" Expression { BinaryExp (Rem      <$ $2) $1 $3 <$ $1 }
    | "-" Expression %prec NEG    { UnaryExp  (Uminus   <$ $2)    $2 <$ $1 }

    ---- Relational
    | Expression "<" Expression  { BinaryExp (LTop      <$ $2) $1 $3 <$ $1 }
    | Expression "=<" Expression { BinaryExp (LEop      <$ $2) $1 $3 <$ $1 }
    | Expression ">" Expression  { BinaryExp (GTop      <$ $2) $1 $3 <$ $1 }
    | Expression ">=" Expression { BinaryExp (GEop      <$ $2) $1 $3 <$ $1 }
    | Expression "=" Expression  { BinaryExp (EQop      <$ $2) $1 $3 <$ $1 }
    | Expression "/=" Expression { BinaryExp (NEop      <$ $2) $1 $3 <$ $1 }
    | Expression "|" Expression  { BinaryExp (Factor    <$ $2) $1 $3 <$ $1 }
    | Expression "!|" Expression { BinaryExp (NotFactor <$ $2) $1 $3 <$ $1 }

Bool :: { At Bool } : boolLit           { unTokenBoolLit  `fmap` $1 }

Char :: { At Char } : charLit           { unTokenCharLit   `fmap` $1 }

Float :: { At Float } : floatLit        { unTokenFloatLit  `fmap` $1 }

Int :: { At Int32 } : intLit            { unTokenIntLit    `fmap` $1 }

String :: { At String } : stringLit     { unTokenStringLit `fmap` $1 }

VarId :: { At String } : varId          { unTokenVarId     `fmap` $1 }

GenId :: { At String } : genId          { unTokenGenId     `fmap` $1 }


{ ------------------------------------------------------------------------------
-- Parser
lexer :: (At Token -> Alex a) -> Alex a
lexer cont = do
    l@(t :@ _) <- alexMonadScan
    case t of
        ErrorUnderflow _ -> do
            lexer cont
        ErrorOverflow _ -> do
            lexer cont
        ErrorUnclosedStringLit s -> do
            cont $ TokenStringLit s <$ l
        ErrorUnexpectedToken _ -> do
            lexer cont
        _ -> cont l

parseError :: At Token -> Alex a
parseError (t :@ (r, c)) = fail $ show r ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n"

-- parseProgram :: String -> (Expression, String)
parseProgram input = runAlex' input parser
}