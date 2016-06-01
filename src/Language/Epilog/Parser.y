{ module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
-- import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Type        
import           Language.Epilog.At
import           Language.Epilog.Lexer
import           Language.Epilog.Context
import           Language.Epilog.Epilog
import           Language.Epilog.Error
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict (RWS, execRWS, get, gets,
                                                 modify, put, tell)
import           Data.Int                       (Int32)
import           Data.Sequence                  (Seq, ViewL ((:<)), (<|), (><),
                                                 (|>))
import qualified Data.Sequence                  as Seq (empty, singleton, viewl)
import           Prelude                        hiding (Either)
--------------------------------------------------------------------------------
}

%name parse
%tokentype { At Token }
%monad { Epilog }
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
    length          { TokenLength       :@ _ }
    "["             { TokenLeftBracket  :@ _ }
    "]"             { TokenRightBracket :@ _ }
    "{"             { TokenLeftBrace    :@ _ }
    "}"             { TokenRightBrace   :@ _ }
    "_"             { TokenUnderscore   :@ _ }

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

%nonassoc length

%% -----------------------------------------------------------------------------
-- Program -----------------------------
Program -- :: { () }
    : TopDefs                       {}

-- Top Level Definitions ---------------
TopDefs -- :: { () }
    : TopDef                        {}
    | TopDefs TopDef                {}

TopDef -- :: {  () }
    : proc GenId "(" Params0 ")" ":-" Insts "."
    { -- % do

    }

    | proc GenId "(" Params0 ")" "->" Type ":-" Insts "."
    { -- % do

    }

    | either GenId ":-" Conts "."
    { -- % do

    }

    | record GenId ":-" Conts "."
    { -- % do

    }

    | Declaration "."
    { -- % do

    }

    | Initialization "."
    { -- % do

    }

GenId -- :: { At String }
    : genId                         { unTokenGenId `fmap` $1 }

Params0 -- :: { Params }
   : {- lambda -}                   {}
   | Params                         {}

Params -- :: { Params }
   : Param                          {}
   | Params "," Param               {}

Param -- :: { Parameter }
   : Type VarId                     {}

Conts -- :: { Conts }
   : Cont                           {}
   | Conts "," Cont                 {}

Cont -- :: { Content }
   : Type VarId                     {}

---- Instructions ----------------------
Insts -- :: { () }
    : Inst                          {}
    | Insts "," Inst                {}

Inst -- :: { () }
    : Declaration                   {}
    | Initialization                {}
    | Assign                        {}
    | Call                          {}
    | If                            {}
    | Case                          {}
    | For                           {}
    | While                         {}
    | read Lval                     {}
    | write Exp                     {}
    | finish                        {}

------ Declaration and Initialization ----
Declaration -- :: { () }
    : Type VarId                    { % do verifyDecl $1 $2 } -- {% do inst (Declaration (pos $1) (item $1) (item $2) Nothing) }

Initialization -- :: { () }
    : Type VarId is Exp             {} -- {% do
                                    --     expr <- gets expression
                                    --     case Seq.viewl expr of
                                    --         x :< xs ->
                                    --             inst (Declaration (pos $1) (item $1) (item $2) (Just x)) }

Type -- :: { At Type }
    : GenId                         { $1 } -- { Type (item $1) Seq.empty <$ $1 }
    --| GenId ArraySize               {} -- { Type (item $1) $2 <$ $1 }

ArraySize -- :: { Seq Int32 }
    : "{" Int "]"                   {} -- { Seq.singleton (item $2) }
    | "[" Int "}"                   {} -- { Seq.singleton (item $2) }
    | ArraySize "{" Int "]"         {} -- { $1 |> (item $3) }
    | ArraySize "[" Int "}"         {} -- { $1 |> (item $3) }

------ Assignment ------------------------
Assign -- :: { () }
    : Lval is Exp                   {} -- {% do
                                    --     expr <- gets expression
                                    --     case Seq.viewl expr of
                                    --         x :< xs ->
                                    --             inst $ Assign (pos $1) (item $1) x }

Lval -- :: { At Lval }
    : VarId                         {}
    | Lval "_" VarId                {}
    | Lval "{" Exp "]"              {}
    | Lval "[" Exp "}"              {}

VarId -- :: { At String }
    : varId                         { unTokenVarId `fmap` $1 }

------ Call ------------------------------
Call -- :: { Instruction }
   : GenId "(" Args ")"             {}

Args -- :: { Exps }
   : {- lambda -}                   {}
   | Args1                          {}

Args1 -- :: { Exps }
   : Exp                            {}
   | Args1 "," Exp                  {}

---- If --------------------------------
If -- :: { Instruction }
   : if Guards end                  {}

Guards -- :: { Guards }
   : Guard                          {}
   | Guards ";" Guard               {}

Guard -- :: { Guard }
   : Exp "->" Insts                 {}

---- Case ------------------------------
Case -- :: { Instruction }
   : case Exp of Sets end           {}

Sets -- :: { Sets }
   : Set                            {}
   | Sets ";" Set                   {}

Elems -- :: { At Exps }
   : Exp                            {}
   | Elems "," Exp                  {}

Set -- :: { Set }
   : Elems "->" Insts               {}

---- For loops -------------------------
For -- :: { Instruction }
   : for      VarId Ranges end      {}
   | for Type VarId Ranges end      {}

Ranges -- :: { Ranges }
   : Range                          {}
   | Ranges ";" Range               {}

Range -- :: { Range }
   : from Exp to Exp "->" Insts     {}

---- While loops -----------------------
While -- :: { Instruction }
   : while Guards end               {}

---- Expressions -------------------------
Exp -- :: { () }
    : "(" Exp ")"                   {}
    | Bool                          {}
    | Char                          {}
    | Int                           {}
    | Float                         {}
    | String                        {}
    | otherwise                     {}

    | Lval                          {} -- {% do
                                    --     let lval = Lval (pos $1) (item $1)
                                    --     modify (\s -> s {expression = lval <| expression s})}

   | GenId "(" Args ")"             {} -- { ECall (pos $1) (item $1) $3 }

    -- Operators
    ---- Logical
    | Exp and     Exp               {}
    | Exp andalso Exp               {}
    | Exp or      Exp               {}
    | Exp orelse  Exp               {}
    | Exp xor     Exp               {}
    |     not     Exp %prec NEG     {}

    ---- Bitwise
    | Exp band Exp                  {}
    | Exp bor  Exp                  {}
    | Exp bsl  Exp                  {}
    | Exp bsr  Exp                  {}
    | Exp bxor Exp                  {}
    |     bnot Exp %prec NEG        {}

    ---- Array / Record / Either
    | length Exp                    {}

    ---- Arithmetic
    | Exp "+" Exp                   {}
    | Exp "-" Exp                   {}
    | Exp "*" Exp                   {}
    | Exp "/" Exp                   {}
    | Exp div Exp                   {}
    | Exp rem Exp                   {}
    |     "-" Exp %prec NEG         {}

    ---- Relational
    | Exp "<"  Exp                  {}
    | Exp "=<" Exp                  {}
    | Exp ">"  Exp                  {}
    | Exp ">=" Exp                  {}
    | Exp "="  Exp                  {}
    | Exp "/=" Exp                  {}
    | Exp "|"  Exp                  {}
    | Exp "!|" Exp                  {}

    Bool -- :: { At Bool }
        : boolLit                   {} -- { unTokenBoolLit   `fmap` $1 }

    Char -- :: { At Char }
        : charLit                   {} -- { unTokenCharLit   `fmap` $1 }

    Int -- :: { At Int32 }
        : intLit                    {} -- { unTokenIntLit    `fmap` $1 }

    Float -- :: { At Float }
        : floatLit                  {} -- { unTokenFloatLit  `fmap` $1 }

    String -- :: { At String }
        : stringLit                 {% string $1 }

{ ------------------------------------------------------------------------------
parseError :: At Token -> Epilog a
parseError (t :@ p) = do
    err $ UnexpectedToken t p
    return undefined
}
