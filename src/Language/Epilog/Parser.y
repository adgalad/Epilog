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
--------------------------------------------------------------------------------
import           Data.Int                        (Int32)
import           Data.Sequence                   (Seq, (<|), (><), (|>))
import qualified Data.Sequence                   as Seq (empty, singleton)
import           Prelude                         hiding (Either)
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
    : TopDefs                       { Program $1 }

-- Top Level Definitions --------------
TopDefs :: { Defs }
    : TopDef                        { Seq.singleton $1 }
    | TopDefs TopDef                { $1 |> $2 }

TopDef :: { Definition }
    : proc GenId "(" Params0 ")" ":-" Insts "."
                                    { ProcD   (pos $1) (item $2) $4 (item (Type "void" Seq.empty <$ $1)) $7 }
    | proc GenId "(" Params0 ")" "->" Type ":-" Insts "."
                                    { ProcD   (pos $1) (item $2) $4 (item $7) $9 }
    | either GenId ":-" Conts "."   { StructD (pos $1) (item $2) Either $4 }
    | record GenId ":-" Conts "."   { StructD (pos $1) (item $2) Record $4 }
    | Type VarId "."                { GlobalD (pos $1) (item $1) (item $2) Nothing }
    | Type VarId is Exp "."         { GlobalD (pos $1) (item $1) (item $2) (Just $4) }

GenId :: { At String }
    : genId                         { unTokenGenId `fmap` $1}

Params0 :: { Params }
    : {- lambda -}                  { Seq.empty }
    | Params                        { $1 }

Params :: { Params }
    : Param                         { Seq.singleton $1 }
    | Params "," Param              { $1 |> $3 }

Param :: { Parameter }
    : Type VarId                    { Parameter (pos $1) (item $1) (item $2) }

Conts :: { Conts }
    : Cont                          { Seq.singleton $1 }
    | Conts "," Cont                { $1 |> $3 }

Cont :: { Content }
    : Type VarId                    { Content (pos $1) (item $1) (item $2) }

-- Instructions ------------------------
Insts :: { Insts }
    : Inst                          { Seq.singleton $1 }
    | Insts "," Inst                { $1   |>   $3 }

Inst :: { Instruction }
    : Declaration                   { $1 }
    | Initialization                { $1 }
    | Assign                        { $1 }
    | Call                          { $1 }
    | If                            { $1 }
    | Case                          { $1 }
    | For                           { $1 }
    | While                         { $1 }
    | read Lval                     { Read   (pos $1) (item $2) }
    | write Exp                     { Write  (pos $1) $2 }
    | finish                        { Finish (pos $1) }

---- Declaration and Initialization ----
Declaration :: { Instruction }
    : Type VarId                    { Declaration (pos $1) (item $1) (item $2) Nothing }

Initialization :: { Instruction }
    : Type VarId is Exp             { Declaration (pos $1) (item $1) (item $2) (Just $4) }

Type :: { At Type }
    : GenId                         { Type (item $1) Seq.empty <$ $1 }
    | GenId ":" ArraySize           { Type (item $1) $3 <$ $1 }

ArraySize :: { Seq Int32 }
    : Int                           { Seq.singleton (item $1) }
    | ArraySize ":" Int             { $1 |> (item $3) }

---- Assignment ------------------------
Assign :: { Instruction }
    : Lval is Exp                   { Assign (pos $1) (item $1) $3 }

Lval :: { At Lval }
    : VarId                         { Variable (item $1)           <$ $1 }
    | Lval "_" VarId                { Member   (item $1) (item $3) <$ $1 }
    | Lval ":" Exp                  { Index    (item $1)       $3  <$ $1 }

VarId :: { At String }
    : varId                         { unTokenVarId `fmap` $1 }

---- Call ------------------------------
Call :: { Instruction }
    : GenId "(" Args ")"            { Call (pos $1) (item $1) $3 }

Args :: { Exps }
    : {- lambda -}                  { Seq.empty }
    | Args1                         { $1 }

Args1 :: { Exps }
    : Exp                           { Seq.singleton $1 }
    | Args1 "," Exp                 { $1 |> $3 }

---- If --------------------------------
If :: { Instruction }
    : if Guards end                 { If (pos $1) $2}

Guards :: { Guards }
    : Guard                         { Seq.singleton $1 }
    | Guards ";" Guard              { $1 |> $3 }

Guard :: { Guard }
    : Exp "->" Insts                { (pos $1, $1, $3) }

---- Case ------------------------------
Case :: { Instruction }
    : case Exp of Sets end          { Case (pos $1) $2 $4 }

Sets :: { Sets }
    : Set                           { Seq.singleton $1 }
    | Sets ";" Set                  { $1 |> $3 }

Elems :: { At Exps }
    : Exp                           { (Seq.singleton $1) :@ (pos $1) }
    | Elems "," Exp                 { ((item $1) |> $3) :@ (pos $1) }

Set :: { Set }
    : Elems "->" Insts              { (pos $1, item $1, $3) }

---- For loops -------------------------
For :: { Instruction }
    : for VarId       Ranges end    { For  (pos $1) (item $2) $3 }
    | for Declaration Ranges end    { ForD (pos $1)       $2  $3 }

Ranges :: { Ranges }
    : Range                         { Seq.singleton $1 }
    | Ranges ";" Range              { $1 |> $3 }

Range :: { Range }
    : from Exp to Exp "->" Insts    { (pos $1, $2, $4, $6) }

---- While loops -----------------------
While :: { Instruction }
    : while Guards end              { While (pos $1) $2 }

-- Expressions -------------------------
Exp :: { Expression }
    : "(" Exp ")"                   { $2 }

    | Bool                          { LitBool   (pos $1) (item $1) }
    | Char                          { LitChar   (pos $1) (item $1) }
    | Int                           { LitInt    (pos $1) (item $1) }
    | Float                         { LitFloat  (pos $1) (item $1) }
    | String                        { LitString (pos $1) (item $1) }
    | otherwise                     { Otherwise (pos $1) }

    | VarId                         { VarId     (pos $1) (item $1) }

    -- Conversion Operators
    | toBoolean   Exp               { Unary (pos $1) ToBoolean   $2 }
    | toCharacter Exp               { Unary (pos $1) ToCharacter $2 }
    | toFloat     Exp               { Unary (pos $1) ToFloat     $2 }
    | toInteger   Exp               { Unary (pos $1) ToInteger   $2 }

    -- Operators
    ---- Logical
    | Exp and     Exp               { Binary (pos $1) And     $1 $3 }
    | Exp andalso Exp               { Binary (pos $1) Andalso $1 $3 }
    | Exp or      Exp               { Binary (pos $1) Or      $1 $3 }
    | Exp orelse  Exp               { Binary (pos $1) Orelse  $1 $3 }
    | Exp xor     Exp               { Binary (pos $1) Xor     $1 $3 }
    |     not     Exp %prec NEG     { Unary  (pos $1) Not     $2 }

    ---- Bitwise
    | Exp band Exp                  { Binary (pos $1) Band $1 $3 }
    | Exp bor  Exp                  { Binary (pos $1) Bor  $1 $3 }
    | Exp bsl  Exp                  { Binary (pos $1) Bsl  $1 $3 }
    | Exp bsr  Exp                  { Binary (pos $1) Bsr  $1 $3 }
    | Exp bxor Exp                  { Binary (pos $1) Bxor $1 $3 }
    |     bnot Exp %prec NEG        { Unary  (pos $1) Bnot $2 }

    ---- Array / Record / Either
    | Exp ":"  Exp                  { Binary (pos $1) Colon      $1 $3 }
    | Exp "_"  Exp                  { Binary (pos $1) Underscore $1 $3 }
    | length Exp                    { Unary  (pos $1) Length $2 }

    ---- Arithmetic
    | Exp "+" Exp                   { Binary (pos $1) Plus     $1 $3 }
    | Exp "-" Exp                   { Binary (pos $1) Minus    $1 $3 }
    | Exp "*" Exp                   { Binary (pos $1) Times    $1 $3 }
    | Exp "/" Exp                   { Binary (pos $1) FloatDiv $1 $3 }
    | Exp div Exp                   { Binary (pos $1) IntDiv   $1 $3 }
    | Exp rem Exp                   { Binary (pos $1) Rem      $1 $3 }
    |     "-" Exp %prec NEG         { Unary  (pos $1) Uminus $2 }

    ---- Relational
    | Exp "<"  Exp                  { Binary (pos $1) LTop $1 $3 }
    | Exp "=<" Exp                  { Binary (pos $1) LEop $1 $3 }
    | Exp ">"  Exp                  { Binary (pos $1) GTop $1 $3 }
    | Exp ">=" Exp                  { Binary (pos $1) GEop $1 $3 }
    | Exp "="  Exp                  { Binary (pos $1) EQop $1 $3 }
    | Exp "/=" Exp                  { Binary (pos $1) NEop $1 $3 }
    | Exp "|"  Exp                  { Binary (pos $1) FAop $1 $3 }
    | Exp "!|" Exp                  { Binary (pos $1) NFop $1 $3 }

Bool :: { At Bool }
    : boolLit                       { unTokenBoolLit   `fmap` $1 }

Char :: { At Char }
    : charLit                       { unTokenCharLit   `fmap` $1 }

Int :: { At Int32 }
    : intLit                        { unTokenIntLit    `fmap` $1 }

Float :: { At Float }
    : floatLit                      { unTokenFloatLit  `fmap` $1 }

String :: { At String }
    : stringLit                     { unTokenStringLit `fmap` $1 }

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

parseError :: (At Token) -> Alex a
parseError (t :@ p) =
    fail $ show p ++ ": Parse error on " ++ show t ++ "\n"

-- parseProgram :: String -> (Exp, String)
parseProgram input = runAlex' input parser
}
