{
module Language.Epilog.Parser
    ( parseProgram
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Program
import           Language.Epilog.At
import           Language.Epilog.Lexer

import           Data.Int                        (Int32)
import           Data.Sequence                   (Seq, (<|), (><), (|>))
import qualified Data.Sequence                   as Seq (empty, singleton)
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
    and             { TokenAnd     :@ _ }
    andalso         { TokenAndalso :@ _ }
    or              { TokenOr      :@ _ }
    orelse          { TokenOrelse  :@ _ }
    xor             { TokenXor     :@ _ }
    not             { TokenNot     :@ _ }

    ---- Bitwise
    band            { TokenBand :@ _ }
    bor             { TokenBor  :@ _ }
    bnot            { TokenBnot :@ _ }
    bsl             { TokenBxor :@ _ }
    bsr             { TokenBxor :@ _ }
    bxor            { TokenBxor :@ _ }

    ---- Array / Record / Either
    length          { TokenLength     :@ _ }
    ":"             { TokenColon      :@ _ }
    "_"             { TokenUnderscore :@ _ }

    ---- Arithmetic
    "+"             { TokenPlus     :@ _ }
    "-"             { TokenMinus    :@ _ }
    "*"             { TokenTimes    :@ _ }
    "/"             { TokenFloatDiv :@ _ }
    div             { TokenIntDiv   :@ _ }
    rem             { TokenRem      :@ _ }

    ---- Relational
    "<"             { TokenLT :@ _ }
    "=<"            { TokenLE :@ _ }
    ">"             { TokenGT :@ _ }
    ">="            { TokenGE :@ _ }
    "="             { TokenEQ :@ _ }
    "/="            { TokenNE :@ _ }
    "|"             { TokenFA :@ _ }
    "!|"            { TokenNF :@ _ }

    -- Control Structures
    end             { TokenEnd       :@ _ }
    for             { TokenFor       :@ _ }
    from            { TokenFrom      :@ _ }
    to              { TokenTo        :@ _ }
    if              { TokenIf        :@ _ }
    otherwise       { TokenOtherwise :@ _ }
    while           { TokenWhile     :@ _ }
    case            { TokenCase      :@ _ }
    of              { TokenOf        :@ _ }

    -- Procedures
    proc            { TokenProcedure :@ _ }
    ":-"            { TokenDefine    :@ _ }
    finish          { TokenFinish    :@ _ }

    -- Composite Types
    either          { TokenEither :@ _ }
    record          { TokenRecord :@ _ }

    -- Conversion
    toBoolean       { TokenToBool  :@ _ }
    toCharacter     { TokenToChar  :@ _ }
    toFloat         { TokenToFloat :@ _ }
    toInteger       { TokenToInt   :@ _ }

    -- Types
    bool            { TokenBoolType   :@ _ }
    char            { TokenCharType   :@ _ }
    int             { TokenIntType    :@ _ }
    float           { TokenFloatType  :@ _ }
    string          { TokenStringType :@ _ }

    -- Punctuation
    ","             { TokenComma     :@ _ }
    "."             { TokenPeriod    :@ _ }
    ";"             { TokenSemicolon :@ _ }
    "->"            { TokenArrow     :@ _ }
    "("             { TokenLeftPar   :@ _ }
    ")"             { TokenRightPar  :@ _ }

    -- Assignment
    is              { TokenIs :@ _ }

    -- IO
    read            { TokenRead  :@ _ }
    write           { TokenWrite :@ _ }

    -- Literals
    boolLit         { TokenBoolLit   _ :@ _ }
    charLit         { TokenCharLit   _ :@ _ }
    intLit          { TokenIntLit    _ :@ _ }
    floatLit        { TokenFloatLit  _ :@ _ }
    stringLit       { TokenStringLit _ :@ _ }

    -- Identifier
    varId           { TokenVarId _ :@ _ }
    genId           { TokenGenId _ :@ _ }

-- Precedence
%right    is

%left     orelse
%left     andalso
%left     or
%left     xor
%left     and
%left     bor
%left     bxor
%left     band

%left     "=" "/="
%nonassoc "|" "!|"
%nonassoc "<" "=<" ">" ">="

%left     bsl bsr
%left     "+" "-"
%left     "*" "/" div rem

%right    NEG
%right    toBoolean toCharacter toFloat toInteger

%left     "_"
%left     ":"
%nonassoc length

%% -----------------------------------------------------------------------------
-- Program -----------------------------
Program :: { Program }
    : TopDecs                       { Program $1 }

-- Top Level Declarations --------------
TopDecs :: { Decs }
    : TopDec                        { Seq.singleton $1 }
    | TopDecs TopDec                { $1 |> $2 }

TopDec :: { Dec }
    : proc GenId "(" Params ")" ":-" Insts "."
                                    { ProcD $2 $4 $7 <$ $1 }
    | either GenId ":-" Decs "."    { EitherD $2 $4 <$ $1 }
    | record GenId ":-" Decs "."    { RecordD $2 $4 <$ $1 }
    | Declaration "."               { GlobalD $1 <$ $1}
    | Initialization "."            { GlobalD $1 <$ $1}

GenId :: { At String }
    : genId                         { unTokenGenId `fmap` $1 }

Params :: { Insts }
    : {- lambda -}                  { Seq.empty }
    | Decs                          { $1 }

Decs :: { Insts }
    : Declaration                   { Seq.singleton $1 }
    | Decs "," Declaration          { $1 |> $3 }

-- Instructions ------------------------
Insts :: { Insts }
    : Inst                          { Seq.singleton $1 }
    | Insts "," Inst                { $1   |>   $3 }

Inst :: { Inst }
    : Declaration                   { $1 }
    | Initialization                { $1 }
    | Assign                        { $1 }
    | Call                          { $1 }
    | If                            { $1 }
    | Case                          { $1 }
    | For                           { $1 }
    | While                         { $1 }
    | read Lval                     { Read $2 <$ $1 }
    | write Exp                     { Write $2 <$ $1 }
    | finish                        { Finish <$ $1 }

---- Declaration and Initialization ----
Declaration :: { Inst }
    : Type VarId                    { Declaration $1 $2 Nothing <$ $1 }

Initialization :: { Inst }
    : Type VarId is Exp             { Declaration $1 $2 (Just $4) <$ $1 }

Type :: { At Type }
    : Atom                          { Type (item $1) Seq.empty <$ $1 }
    | Atom ":" ArraySize            { Type (item $1) $3 <$ $1 }

Atom :: { At Atom }
    : bool                          { BoolT     <$ $1 }
    | char                          { CharT     <$ $1 }
    | float                         { FloatT    <$ $1 }
    | int                           { IntT      <$ $1 }
    | string                        { StringT   <$ $1 }
    | GenId                         { UserT `fmap` $1 }

ArraySize :: { Seq Int32 }
    : Int                           { Seq.singleton (item $1) }
    | ArraySize ":" Int             { $1 |> (item $3) }

---- Assignment ------------------------
Assign :: { Inst }
    : Lval is Exp                   { Assign (item $1) $3 <$ $1 }

Lval :: { At Lval }
    : VarId                         { Variable         $1 <$ $1 }
    | Lval "_" VarId                { Member (item $1) $3 <$ $1 }
    | Lval ":" Exp                  { Entry  (item $1) $3 <$ $1 }

VarId :: { At String }
    : varId                         { unTokenVarId `fmap` $1 }

---- Call ------------------------------
Call :: { Inst }
    : GenId "(" Args ")"            { Call $1 $3 <$ $1 }

Args :: { Exps }
    : {- lambda -}                  { Seq.empty }
    | Args1                         { $1 }

Args1 :: { Exps }
    : Exp                           { Seq.singleton $1 }
    | Args1 "," Exp                 { $1 |> $3 }

---- If --------------------------------
If :: { Inst }
    : if Guards end                 { If $2 <$ $1}

Guards :: { Guards }
    : Guard                         { Seq.singleton $1 }
    | Guards ";" Guard              { $1 |> $3 }

Guard :: { Guard }
    : Exp "->" Insts                { ($1, $3) }

---- Case ------------------------------
Case :: { Inst }
    : case Exp of Sets end          { Case $2 $4 <$ $1 }

Sets :: { Sets }
    : Set                           { Seq.singleton $1 }
    | Sets ";" Set                  { $1 |> $3 }

Exps :: { Exps }
    : Exp                           { Seq.singleton $1 }
    | Exps "," Exp                  { $1 |> $3 }

Set :: { Set }
    : Exps "->" Insts               { ($1, $3) }

---- For loops -------------------------
For :: { Inst }
    : for VarId       Ranges end    { For  (VarId `fmap` $2) $3 <$ $1 }
    | for Declaration Ranges end    { ForD $2 $3 <$ $1 }

Ranges :: { Ranges }
    : Range                         { Seq.singleton $1 }
    | Ranges ";" Range              { $1 |> $3 }

Range :: { Range }
    : from Exp to Exp "->" Insts    { ($2, $4, $6) }

---- While loops -----------------------
While :: { Inst }
    : while Conds end               { While $2 <$ $1 }

Conds :: { Conds }
    : Cond                          { Seq.singleton $1 }
    | Conds ";" Cond                { $1 |> $3 }

Cond :: { Cond }
    : Exp "->" Insts                { ($1, $3) }

-- Expressions -------------------------
Exp :: { Exp }
    : "(" Exp ")"                   { $2 }

    | Bool                          { LitBool   `fmap` $1 }
    | Char                          { LitChar   `fmap` $1 }
    | Int                           { LitInt    `fmap` $1 }
    | Float                         { LitFloat  `fmap` $1 }
    | String                        { LitString `fmap` $1 }
    | otherwise                     { Otherwise   <$   $1 }

    | VarId                         { VarId     `fmap` $1 }

    -- Conversion Operators
    | toBoolean   Exp               { Unary (ToBoolean   <$ $1) $2 <$ $1 }
    | toCharacter Exp               { Unary (ToCharacter <$ $1) $2 <$ $1 }
    | toFloat     Exp               { Unary (ToFloat     <$ $1) $2 <$ $1 }
    | toInteger   Exp               { Unary (ToInteger   <$ $1) $2 <$ $1 }

    -- Operators
    ---- Logical
    | Exp and     Exp               { Binary (And     <$ $2) $1 $3 <$ $1 }
    | Exp andalso Exp               { Binary (Andalso <$ $2) $1 $3 <$ $1 }
    | Exp or      Exp               { Binary (Or      <$ $2) $1 $3 <$ $1 }
    | Exp orelse  Exp               { Binary (Orelse  <$ $2) $1 $3 <$ $1 }
    | Exp xor     Exp               { Binary (Xor     <$ $2) $1 $3 <$ $1 }
    |     not     Exp %prec NEG     { Unary  (Not     <$ $1)    $2 <$ $1 }

    ---- Bitwise
    | Exp band Exp                  { Binary (Band <$ $2) $1 $3 <$ $1 }
    | Exp bor  Exp                  { Binary (Bor  <$ $2) $1 $3 <$ $1 }
    | Exp bsl  Exp                  { Binary (Bsl  <$ $2) $1 $3 <$ $1 }
    | Exp bsr  Exp                  { Binary (Bsr  <$ $2) $1 $3 <$ $1 }
    | Exp bxor Exp                  { Binary (Bxor <$ $2) $1 $3 <$ $1 }
    |     bnot Exp %prec NEG        { Unary  (Bnot <$ $1)    $2 <$ $1 }

    ---- Array / Record / Either
    | Exp ":"  Exp                  { Binary (Colon      <$ $2) $1 $3 <$ $1 }
    | Exp "_"  Exp                  { Binary (Underscore <$ $2) $1 $3 <$ $1 }
    | length Exp                    { Unary  (Length     <$ $1)    $2 <$ $1 }

    ---- Arithmetic
    | Exp "+" Exp                   { Binary (Plus     <$ $2) $1 $3 <$ $1 }
    | Exp "-" Exp                   { Binary (Minus    <$ $2) $1 $3 <$ $1 }
    | Exp "*" Exp                   { Binary (Times    <$ $2) $1 $3 <$ $1 }
    | Exp "/" Exp                   { Binary (FloatDiv <$ $2) $1 $3 <$ $1 }
    | Exp div Exp                   { Binary (IntDiv   <$ $2) $1 $3 <$ $1 }
    | Exp rem Exp                   { Binary (Rem      <$ $2) $1 $3 <$ $1 }
    |     "-" Exp %prec NEG         { Unary  (Uminus   <$ $2)    $2 <$ $1 }

    ---- Relational
    | Exp "<"  Exp                  { Binary (LTop <$ $2) $1 $3 <$ $1 }
    | Exp "=<" Exp                  { Binary (LEop <$ $2) $1 $3 <$ $1 }
    | Exp ">"  Exp                  { Binary (GTop <$ $2) $1 $3 <$ $1 }
    | Exp ">=" Exp                  { Binary (GEop <$ $2) $1 $3 <$ $1 }
    | Exp "="  Exp                  { Binary (EQop <$ $2) $1 $3 <$ $1 }
    | Exp "/=" Exp                  { Binary (NEop <$ $2) $1 $3 <$ $1 }
    | Exp "|"  Exp                  { Binary (FAop <$ $2) $1 $3 <$ $1 }
    | Exp "!|" Exp                  { Binary (NFop <$ $2) $1 $3 <$ $1 }

Bool :: { At Bool }
    : boolLit                       { unTokenBoolLit    `fmap` $1 }

Char :: { At Char }
    : charLit                       { unTokenCharLit    `fmap` $1 }

Int :: { At Int32 }
    : intLit                        { unTokenIntLit     `fmap` $1 }

Float :: { At Float }
    : floatLit                      { unTokenFloatLit   `fmap` $1 }

String :: { At String }
    : stringLit                     { unTokenStringLit  `fmap` $1 }

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
parseError (t :@ p) =
    fail $ show p ++ ": Parse error on " ++ show t ++ "\n"

-- parseProgram :: String -> (Exp, String)
parseProgram input = runAlex' input parser
}
