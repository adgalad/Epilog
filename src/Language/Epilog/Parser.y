{ module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
-- import qualified Language.Epilog.AST.AST         as AST
import           Language.Epilog.Type
import           Language.Epilog.At
import           Language.Epilog.Lexer
import           Language.Epilog.Context
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.SymbolTable
import           Language.Epilog.Joy
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict (RWS, execRWS, get, gets,
                                                 modify, put, tell)
import           Control.Monad                  (unless)
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
    answer          { TokenAnswer    :@ _ }

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
    : PREPARE OPEN TopDefs CLOSE     {}

PREPARE
    : {- lambda -}
    {% prepare }

OPEN
    : {- lambda -}
    {% symbols %= openScope (Position 1 1) }

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
        current     .= Just (item $2 :@ pos $1)
        curkind     .= Just (item $1)
        offset      %= (0:)
        structSize  .= 0
        structAlign .= 0
    }
StructKind
    : either { EitherK :@ (pos $1) }
    | record { RecordK :@ (pos $1) }

Procedure
    : Procedure1 Procedure2 Procedure3
    {% storeProcedure' $3  }
Procedure1
    : proc GenId OPENF( "(" ) Params0 ")"
    {% current .= Just (item $2 :@ pos $1) }
Procedure2
    : {- lambda -}
    {% storeProcedure EpVoid }
    | "->" Type
    {% storeProcedure (item $2) }
Procedure3
    : OPENF( ":-" ) Insts CLOSE(CLOSE( "." ))
    { $2 }


OPENF(TOKEN)
    : TOKEN
    {% do
        symbols %= openScope (pos $1)
        offset %= (0:)
        return $1
    }

OPEN(TOKEN)
    : TOKEN
    {% do
        symbols %= openScope (pos $1)
        offset %= (\(x:xs) -> (x:x:xs))
        return $1
    }

CLOSE(TOKEN)
    : TOKEN
    {% do
        symbols %= \st ->
            case goUp (closeScope (pos $1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return $1
    }

GenId
    : genId
    { unTokenGenId `fmap` $1 }

Params0
    : {- lambda -}
    {}
    | Params
    {}

Params
    : Param
    {}
    | Params "," Param
    {}

Param
    : Type VarId
    {% checkDeclaration $1 $2 }

Conts
    : Cont
    {}
    | Conts "," Cont
    {}

Cont
    : Type VarId
    {% verifyField $2 (item $1) }


---- Instructions ----------------------
Insts
    : Inst
    { $1 }
    | Insts "," Inst
    {% instructions $1 $3 }

Inst
    : Declaration                   { $1 }
    | Initialization                { $1 }
    | Assign                        { $1 }
    | Call                          { $1 }
    | If                            { $1 }
    | Case                          { $1 }
    | For                           { $1 }
    | While                         { $1 }

    | finish
    {% checkAnswer (pos $1) joy  }

    | answer Exp
    {% checkAnswer (pos $1) $2  }

    | write Exp
    {% checkWrite (pos $1) $2  }

    | read Lval
    {% checkRead (pos $1) $2  }

------ Declaration and Initialization ----
Declaration
    : Type VarId
    {% checkDeclaration $1 $2 } -- A declaration must not appear on the AST

Initialization
    : Type VarId is Exp
    {% checkInitialization $1 $2 $4 }

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
    { buildArray $2 `fmap` $1 }

TBase
    : TCore
    { $1 }

    | TPointers TCore
    {% do
        t <- buildPointers (item $1) (item $2)
        return $ t :@ pos $1
    }

TCore
    : GenId
    {% do
        t <- lookupType (item $1)
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
    { Seq.empty }

    | TSizes TSize
    { $1 |> $2 }

TSize
    : "[" Int "}"                      { (      0    , item $2 - 1) }
    | "[" Int "," Int "]"              { (item $2    , item $4    ) }
    | "[" Int "," Int "}"              { (item $2    , item $4 - 1) }
    | "{" Int "," Int "]"              { (item $2 + 1, item $4    ) }
    | "{" Int "," Int "}"              { (item $2 + 1, item $4 - 1) }

------ Assignment ------------------------
Assign
    : Lval is Exp
    {% checkAssign $1 $3 }


Lval
    : VarId
    {% checkVariable $1 }

    | Lval "[" Exp "]"
    {% checkSubindex $1 $3 }

    | Lval "_" VarId
    {% $1 `getField` $3 }

    | Lval "^"
    {% deref $1 }


VarId
    : varId
    { unTokenVarId `fmap` $1 }

------ Call ------------------------------
Call
    : GenId "(" Args ")"
    {% checkCall $1 $3 }

Args
    : {- lambda -}
    { Seq.empty }
    | Args1
    { $1 }

Args1
    : Arg
    { Seq.singleton $1 }
    | Args1 "," Arg
    { $1 |> $3 }

Arg
    : Exp
    { $1 }

---- If --------------------------------
If
    : if Guards CLOSE( end )
    {% checkIf (pos $1) $2 }

Guards
    : Guard
    { $1 }
    | Guards CLOSE( ";" ) Guard
    {% checkGuards $1 $3 }

Guard
    : GuardCond OPEN( "->" ) Insts
    {% checkGuard $1 $3 }

GuardCond
    : Exp
    {% checkGuardCond $1 }


---- Case ------------------------------
Case
    : case CaseExp of Sets CLOSE( end )
    {% do
        checkCase (pos $1) $2 $4
    }

CaseExp
    : Exp
    {% checkCaseExp $1 }

Sets
    : Set
    { $1 }
    | Sets CLOSE( ";" ) Set
    {% checkSets $1 $3 }

Set
    : Elems OPEN( "->" ) Insts
    {% checkSet $1 $3 }

Elems
    : Elem
    { $1 }
    | Elems "," Elem
    {% checkSetElems $1 $3 }

Elem
    : Int
    {% checkIntElem $1 }
    | Char
    {% checkCharElem $1 }


---- For loops -------------------------
For
    :       for   ForV Ranges CLOSE( end )
    {% checkFor (pos $1) $2 $3 }

    | OPEN( for ) ForD Ranges CLOSE( CLOSE( end ) )
    {% checkFor (pos $1) $2 $3 }

ForD -- It could be a Declaration
    : Type VarId
    {% checkForD $1 $2 }

ForV -- Or a var
    : VarId
    {% checkForV $1 }

Ranges
    : Range
    { $1 }
    | Ranges CLOSE( ";" ) Range
    {% checkRanges $1 $3 }

Range
    : Range1 OPEN( "->" ) Insts
    {% checkRange $1 $3 }

Range1
    : from Exp to Exp
    {% checkRangeLimits $2 $4 }


---- While loops -----------------------
While
    : while Guards CLOSE( end )
    {% checkWhile (pos $1) $2 }

---- Expressions -------------------------
Exp
    : "(" Exp ")"
    { $2 }
    | Bool
    {% return $ joy
        { jType = boolT
        , jPos  = pos $1
        , jExps = Seq.singleton $ LitBool (pos $1) (item $1)
        }
    }
    | Char
    {% return $ joy
        { jType = charT
        , jPos  = pos $1
        , jExps = Seq.singleton $ LitChar (pos $1) (item $1)
        }
    }
    | Int
    {% return $ joy
        { jType = intT
        , jPos  = pos $1
        , jExps = Seq.singleton $ LitInt (pos $1) (item $1)
        }
    }
    | Float
    {% return $ joy
        { jType = floatT
        , jPos  = pos $1
        , jExps = Seq.singleton $ LitFloat (pos $1) (item $1)
        }
    }
    | String
    {% return $ joy
        { jType = stringT
        , jPos  = pos $1
        , jExps = Seq.singleton $ LitString (pos $1) (item $1)
        }
    }

    -- | otherwise                  { EpVoid   }

    | Lval
    {% do
        -- lval <- AST.topLval
        -- AST.insertExpr lval
        return $1
    }

    | Call
    {% expCall $1 }

    -- Operators
    ---- Logical
    | Exp and     Exp               {% checkBinOp (pos $2) And     $1 $3 }
    | Exp andalso Exp               {% checkBinOp (pos $2) Andalso $1 $3 }
    | Exp or      Exp               {% checkBinOp (pos $2) Or      $1 $3 }
    | Exp orelse  Exp               {% checkBinOp (pos $2) Orelse  $1 $3 }
    | Exp xor     Exp               {% checkBinOp (pos $2) Xor     $1 $3 }
    |     not     Exp %prec NEG     {% checkUnOp  (pos $1) Not     $2 }

    ---- Bitwise
    | Exp band Exp                  {% checkBinOp (pos $2) Band $1 $3 }
    | Exp bor  Exp                  {% checkBinOp (pos $2) Bor  $1 $3 }
    | Exp bsl  Exp                  {% checkBinOp (pos $2) Bsl  $1 $3 }
    | Exp bsr  Exp                  {% checkBinOp (pos $2) Bsr  $1 $3 }
    | Exp bxor Exp                  {% checkBinOp (pos $2) Bxor $1 $3 }
    |     bnot Exp %prec NEG        {% checkUnOp  (pos $1) Bnot $2 }

    ---- Arithmetic
    | Exp "+" Exp                   {% checkBinOp (pos $2) Plus     $1 $3 }
    | Exp "-" Exp                   {% checkBinOp (pos $2) Minus    $1 $3 }
    | Exp "*" Exp                   {% checkBinOp (pos $2) Times    $1 $3 }
    | Exp "/" Exp                   {% checkBinOp (pos $2) FloatDiv $1 $3 }
    | Exp div Exp                   {% checkBinOp (pos $2) IntDiv   $1 $3 }
    | Exp rem Exp                   {% checkBinOp (pos $2) Rem      $1 $3 }
    |     "-" Exp %prec NEG         {% checkUnOp  (pos $1) Uminus   $2 }

    ---- Relational
    | Exp "<"  Exp                  {% checkBinOp (pos $2) LTop $1 $3 }
    | Exp "=<" Exp                  {% checkBinOp (pos $2) LEop $1 $3 }
    | Exp ">"  Exp                  {% checkBinOp (pos $2) GTop $1 $3 }
    | Exp ">=" Exp                  {% checkBinOp (pos $2) GEop $1 $3 }
    | Exp "="  Exp                  {% checkBinOp (pos $2) EQop $1 $3 }
    | Exp "/=" Exp                  {% checkBinOp (pos $2) NEop $1 $3 }
    | Exp "|"  Exp                  {% checkBinOp (pos $2) FAop $1 $3 }
    | Exp "!|" Exp                  {% checkBinOp (pos $2) NFop $1 $3 }

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
