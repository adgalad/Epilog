{ module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
-- import           Language.Epilog.AST.Program
import           Language.Epilog.Type
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
import           Prelude                        hiding (Either, lookup)
import           Control.Lens                   ((%=), use, (.=), (+=), (<~))
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
    -- otherwise       { TokenOtherwise :@ _ }
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

    | Struct ":-" Conts "."
    {% declStruct }

Struct
    : StructKind GenId
    {% do
        current .= Just (item $2 :@ pos $1)
        curkind .= Just (item $1)
    }
StructKind
    : either { EitherK :@ (pos $1) }
    | record { RecordK :@ (pos $1) }

Procedure
    : Procedure1 Procedure2 Procedure3 {}
Procedure1
    : proc GenId OPEN( "(" ) Params0 ")"
    {%
        current .= Just (item $2 :@ pos $1)
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
   : Cont                           {}
   | Conts "," Cont                 {}

Cont
   : Type VarId                     {% verifyField $2 (item $1) }


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
    : Type VarId is Exp
    {% do
        declVar $1 $2
        if item $1 /= item $4
            then err $ InvalidAssign (item $1) (item $4) (pos $1)
            else return ()
    }

Type
    : Type1
    {% case $1 of
        (Undef tname :@ p) -> do
            err $ UndefinedType tname p
            return (None :@ p)
        _ -> return $1
    }

Type1
    : TBase TSizes
    { $2 :@ $1 }

TBase
    : TCore
    {% do
        curtype .= item $1
        return (pos $1)
    }

    | TPointers TCore
    {% do
        curtype <~ buildPointers (item $1) (item $2)
        return (pos $1)
    }

TCore
    : GenId
    {% do
        t <- findType (item $1)
        return (t :@ pos $1)
    }

    | "(" Type1 ")"
    { (item $2) :@ (pos $1) }

TPointers
    : "^"
    { 1 :@ (pos $1) }

    | TPointers "^"
    { (+1) `fmap` $1 }

TSizes
    : {- lambda -}
    {% do
        t <- use curtype
        curtype .= None
        return t
    }

    | TSizes TSize
    {% buildArray $1 $2 }

TSize
    : "[" Int "}"                      { (      0    , item $2 - 1) }
    | "[" Int "," Int "]"              { (item $2    , item $4    ) }
    | "[" Int "," Int "}"              { (item $2    , item $4 - 1) }
    | "{" Int "," Int "]"              { (item $2 + 1, item $4    ) }
    | "{" Int "," Int "}"              { (item $2 + 1, item $4 - 1) }

------ Assignment ------------------------
Assign
    : Lval is Exp
    {% do
        symbs <- use symbols
        if item $1 /= item $3
            then err $ InvalidAssign (item $1) (item $3) (pos $1)
            else return ()
    }


Lval
    : VarId
    { %do
        isSymbol' $1
        findTypeOfSymbol $1
    }

    | Lval "[" Exp "]"
    {% checkArray $1 (item $3)  }

    | Lval "_" VarId
    {% $1 `getField` $3 }


VarId
    : varId                         { unTokenVarId `fmap` $1 }

------ Call ------------------------------
Call
   : GenId "(" Args ")"             { checkCall $1 $3 }

Args
   : {- lambda -}                   { Seq.empty }
   | Args1                          { $1 }

Args1
   : Arg                            { Seq.singleton $1 }
   | Args1 "," Arg                  { $1 |> $3 }

Arg
    : Exp                           { item $1 }

---- If --------------------------------
If
   : if Guards CLOSE( end )         {}

Guards
   : Guard                          {}
   | Guards CLOSE( ";" ) Guard      {}

Guard
   : Exp OPEN( "->" ) Insts
                                    {% if item $1 /= boolT
                                            then err $ InvalidGuard (name $ item $1) (pos $1)
                                            else return ()
                                    }

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
    : Type VarId
    {% do declVar $1 $2}

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
   {% checkFor $2 $4 }


---- While loops -----------------------
While
   : while Guards CLOSE( end )      {}

---- Expressions -------------------------
Exp
    : "(" Exp ")"                   { $2 }
    | Bool                          { boolT   :@ pos $1 }
    | Char                          { charT   :@ pos $1 }
    | Int                           { intT    :@ pos $1 }
    | Float                         { floatT  :@ pos $1 }
    | String                        { stringT :@ pos $1 }
    -- | otherwise                     { voidT   }

    | Lval                          { $1 }

    | GenId "(" Args ")"            {% checkCall $1 $3 }

    -- Operators
    ---- Logical
    | Exp and     Exp               {% checkBinOp And      (pos $1) (item $1) (item $3) }
    | Exp andalso Exp               {% checkBinOp Andalso  (pos $1) (item $1) (item $3) }
    | Exp or      Exp               {% checkBinOp Or       (pos $1) (item $1) (item $3) }
    | Exp orelse  Exp               {% checkBinOp Orelse   (pos $1) (item $1) (item $3) }
    | Exp xor     Exp               {% checkBinOp Xor      (pos $1) (item $1) (item $3) }
    |     not     Exp %prec NEG     {% checkUnOp  Not      (pos $1) (item $2) }

    ---- Bitwise
    | Exp band Exp                  {% checkBinOp Band     (pos $1) (item $1) (item $3) }
    | Exp bor  Exp                  {% checkBinOp Bor      (pos $1) (item $1) (item $3) }
    | Exp bsl  Exp                  {% checkBinOp Bsl      (pos $1) (item $1) (item $3) }
    | Exp bsr  Exp                  {% checkBinOp Bsr      (pos $1) (item $1) (item $3) }
    | Exp bxor Exp                  {% checkBinOp Bxor     (pos $1) (item $1) (item $3) }
    |     bnot Exp %prec NEG        {% checkUnOp  Bnot     (pos $1) (item $2) }

    ---- Arithmetic
    | Exp "+" Exp                   {% checkBinOp Plus     (pos $1) (item $1) (item $3) }
    | Exp "-" Exp                   {% checkBinOp Minus    (pos $1) (item $1) (item $3) }
    | Exp "*" Exp                   {% checkBinOp Times    (pos $1) (item $1) (item $3) }
    | Exp "/" Exp                   {% checkBinOp FloatDiv (pos $1) (item $1) (item $3) }
    | Exp div Exp                   {% checkBinOp IntDiv   (pos $1) (item $1) (item $3) }
    | Exp rem Exp                   {% checkBinOp Rem      (pos $1) (item $1) (item $3) }
    |     "-" Exp %prec NEG         {% checkUnOp  Uminus   (pos $1) (item $2) }

    ---- Relational
    | Exp "<"  Exp                  {% checkBinOp LTop     (pos $1) (item $1) (item $3) }
    | Exp "=<" Exp                  {% checkBinOp LEop     (pos $1) (item $1) (item $3) }
    | Exp ">"  Exp                  {% checkBinOp GTop     (pos $1) (item $1) (item $3) }
    | Exp ">=" Exp                  {% checkBinOp GEop     (pos $1) (item $1) (item $3) }
    | Exp "="  Exp                  {% checkBinOp EQop     (pos $1) (item $1) (item $3) }
    | Exp "/=" Exp                  {% checkBinOp NEop     (pos $1) (item $1) (item $3) }
    | Exp "|"  Exp                  {% checkBinOp FAop     (pos $1) (item $1) (item $3) }
    | Exp "!|" Exp                  {% checkBinOp NFop     (pos $1) (item $1) (item $3) }

Bool
    : boolLit                       { unTokenBoolLit `fmap` $1 }

Char
    : charLit                       { unTokenCharLit `fmap` $1 }

Int
    : intLit                        { unTokenIntLit  `fmap` $1 }

Float
    : floatLit                      { unTokenFloatLit `fmap` $1 }

String
    : stringLit
    {% do
        string $1
        return $ unTokenStringLit `fmap` $1
    }

{ ------------------------------------------------------------------------------
parseError :: At Token -> Epilog a
parseError (t :@ p) = do
    err $ UnexpectedToken t p
    return undefined
}
