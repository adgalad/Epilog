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
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict (RWS, execRWS, get, gets,
                                                 modify, put, tell)
import           Data.Int                       (Int32)
import           Data.Sequence                  (Seq, ViewL ((:<)), (<|), (><),
                                                 (|>))
import qualified Data.Sequence                  as Seq (empty, singleton, viewl)
import           Prelude                        hiding (Either)
import           Control.Lens                   ((%=), use, (.=))
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
    "^"             { TokenCaret        :@ _ }
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
Program
    : OPEN TopDefs CLOSE            {}

OPEN
    : {- lambda -}
    {%
        symbols %= openScope (Position (1,1))
    }

CLOSE
    : {- lambda -}
    {%
        symbols %= \st ->
            case goUp (closeScope EOFP st) of
                Left  _   -> st
                Right st' -> st'
    }

-- Top Level Definitions ---------------
TopDefs
    : TopDef                        {}
    | TopDefs TopDef                {}

TopDef
    : Declaration "."               {}
    | Initialization "."            {}

    | Procedure                     {}

    | either GenId OPEN( ":-" ) Conts CLOSE( "." )
    {% do
        declStruct $2 $4 Either
    }

    | record GenId OPEN( ":-" ) Conts CLOSE( "." )
    { % do
        declStruct $2 $4 Record
    }

Procedure
    : Procedure1 Procedure2 Procedure3 {}
Procedure1
    : proc GenId OPEN( "(" ) Params0 ")"
    {%
        current .= Just (pos $1, item $2)
    }
Procedure2
    : {- lambda -}
    {% do
        storeProcedure voidT
    }
    | "->" Type
    {% do
        storeProcedure (item $2)
    }
Procedure3
    : OPEN ( ":-" ) Insts CLOSE(CLOSE( "." ))
    {}


OPEN(TOKEN)
    : TOKEN
    {% do
        symbols %= openScope (pos $1)
        return $1
    }

CLOSE(TOKEN)
    : TOKEN
    {% do
        symbols %= \st ->
            case goUp (closeScope (pos $1) st) of
                Left  _   -> st
                Right st' -> st'
        return $1
    }

GenId
    : genId                         { unTokenGenId `fmap` $1 }

Params0
   : {- lambda -}                   {}
   | Params                         {}

Params
   : Param                          {}
   | Params "," Param               {}

Param
   : Type VarId                     {% do declVar $1 $2}

Conts
   : Cont                           { Seq.singleton $1}
   | Conts "," Cont                 { $1 |> $3}

Cont
   : Type VarId                     { (item $2, item $1) }


---- Instructions ----------------------
Insts
    : Inst                          {}
    | Insts "," Inst                {}

Inst
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
Declaration
    : Type VarId                    { % do declVar $1 $2 }

Initialization
    : Type VarId is Exp             { % do declVar $1 $2 }
                                    -- ignoring $4 for now

Type
    : GenId                         {% findType     $1 }
    | Type ArraySize                {% buildArray   $1 $2 }
    | "^" Type                      {% buildPointer $2 }
    | "(" Type ")"                  { $2 }


ArraySize
    : "{" Int "]"                   { item $2 }
    | "[" Int "}"                   { item $2 }

------ Assignment ------------------------
Assign
    : Lval is Exp                   {} -- {% do
                                    --     expr <- gets expression
                                    --     case Seq.viewl expr of
                                    --         x :< xs ->
                                    --             inst $ Assign (pos $1) (item $1) x }

Lval
    : VarId                         { %do isSymbol' $1 }
    | Lval "_" VarId                { %do isSymbol' $3 }
    | Lval "{" Exp "]"              {}
    | Lval "[" Exp "}"              {}

VarId
    : varId                         { unTokenVarId `fmap` $1 }

------ Call ------------------------------
Call
   : GenId "(" Args ")"             {}

Args
   : {- lambda -}                   {}
   | Args1                          {}

Args1
   : Exp                            {}
   | Args1 "," Exp                  {}

---- If --------------------------------
If
   : if Guards CLOSE( end )         {}

Guards
   : Guard                          {}
   | Guards CLOSE( ";" ) Guard      {}

Guard
   : Exp OPEN( "->" ) Insts
                                    {}

---- Case ------------------------------
Case
   : case Exp of Sets CLOSE( end )  {}

Sets
   : Set                            {}
   | Sets CLOSE( ";" ) Set          {}

Elems
   : Exp                            {}
   | Elems "," Exp                  {}

Set
   : Elems OPEN( "->" ) Insts       {}

---- For loops -------------------------

ForD -- It could be Declaration
    : Type VarId                    {% do declVar $1 $2}

For
   : for      VarId Ranges CLOSE( end )
   { % do isSymbol' $2 }

   | OPEN( for ) ForD Ranges CLOSE( CLOSE( end ) )
   { }

Ranges
   : Range                          {}
   | Ranges CLOSE( ";" ) Range      {}

Range
   : from Exp to Exp OPEN( "->" ) Insts
                                    {}

---- While loops -----------------------
While
   : while Guards CLOSE( end )      {}

---- Expressions -------------------------
Exp
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

    Bool
        : boolLit                   {} -- { unTokenBoolLit   `fmap` $1 }

    Char
        : charLit                   {} -- { unTokenCharLit   `fmap` $1 }

    Int
        : intLit                    { unTokenIntLit `fmap` $1 }


    Float
        : floatLit                  {} -- { unTokenFloatLit  `fmap` $1 }

    String
        : stringLit                 {% string $1 }

{ ------------------------------------------------------------------------------
parseError :: At Token -> Epilog a
parseError (t :@ p) = do
    err $ UnexpectedToken t p
    return undefined
}
