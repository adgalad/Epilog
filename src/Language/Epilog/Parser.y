{ module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
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
    {% current .= Nothing }

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
    : Type VarId                     {% do checkDeclVar $1 $2}

Conts
    : Cont                           {}
    | Conts "," Cont                 {}

Cont
    : Type VarId                     {% verifyField $2 (item $1) }


---- Instructions ----------------------
Insts
    : Inst
    { $1 }
    | Insts "," Inst
    { checkBoth $1 $3 }

Inst
    : Declaration                   { $1 }     -- returns At Type
    | Initialization                { $1 }     -- returns At Type
    | Assign                        { $1 }     -- returns At Type
    | Call                          { $1 }     -- returns At Type
    | If                            { $1 }     -- returns At Type
    | Case                          { $1 }     -- returns At Type
    | For                           { $1 }     -- Does not return At Type
    | While                         { $1 }     -- Does not return At Type

    | finish
    {% checkAnswer voidT (pos $1) }        -- returns At Type

    | answer Exp
    {% checkAnswer (item $2) (pos $1) }    -- returns At Type

    | write Exp
    {% checkWrite (item $2) (pos $1) }     -- returns At Type

    | read Lval
    {% checkRead (item $2) (pos $1) }      -- returns At Type

------ Declaration and Initialization ----
Declaration
    : Type VarId
    {% checkDeclVar $1 $2 }

Initialization
    : Type VarId is Exp
    {% checkInit $1 $2 $4 }

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
    { buildPointers (item $1) (item $2) :@ (pos $1) }

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
    { %do
        isSymbol' $1
        findTypeOfSymbol $1
    }

    | Lval "[" Exp "]"
    {% checkArray $1 (item $3)  }

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
    { item $1 }

---- If --------------------------------
If
    : if Guards CLOSE( end )
    { item $2 :@ pos $1 }

Guards
    : Guard
    { $1 }
    | Guards CLOSE( ";" ) Guard
    { checkBoth $1 $3 }

Guard
    : GuardCond OPEN( "->" ) Insts
    { checkBoth $1 $3 }

GuardCond
    : Exp
    {% do
        if item $1 == boolT
            then return $ voidT :@ pos $1
            else do
                err $ InvalidGuard (item $1) (pos $1)
                return $ None :@ pos $1
    }

---- Case ------------------------------
Case
    : case CaseExp of Sets CLOSE( end )
    {% do
        caseTypes %= tail
        return $ checkBoth $2 $4
    }

CaseExp
    : Exp
    {% if (item $1) `elem` [intT, charT]
        then do
            caseTypes %= ($1 :)
            return $1
        else do
            caseTypes %= ((None :@ pos $1) :)
            err $ BadCaseExp (item $1) (pos $1)
            return $ None :@ pos $1
    }

Sets
    : Set
    { $1 }
    | Sets CLOSE( ";" ) Set
    { checkBoth $1 $3 }

Set
    : Elems OPEN( "->" ) Insts
    { checkBoth $1 $3 }

Elems
    : Elem
    { $1 }
    | Elems "," Elem
    { checkBoth $1 $3 }

Elem
    : Int
    {% do
        ((ct :@ p):_) <- use caseTypes
        if (ct == intT)
            then return $ intT :@ pos $1
            else do
                err $ BadCaseCharElem p (item $1) (pos $1)
                return $ None :@ (pos $1)
    }

    | Char
    {% do
        ((ct :@ p):_) <- use caseTypes
        if (ct == charT)
            then return $ charT :@ pos $1
            else do
                err $ BadCaseIntElem p (item $1) (pos $1)
                return $ None :@ (pos $1)
    }

---- For loops -------------------------
For
    :       for   ForV Ranges CLOSE( end )
    {% do
        forVars %= tail
        return $ checkBoth $2 $3
    }

    | OPEN( for ) ForD Ranges CLOSE( CLOSE( end ) )
    {% do
        forVars %= tail
        return $ checkBoth $2 $3
    }

ForD -- It could be Declaration
    : Type VarId
    {% do
        if (item $1) `elem` [intT, charT]
            then do
                t :@ p <- checkDeclVar $1 $2
                case t of
                    None -> do
                        forVars %= (($2, None):)
                        return $ None :@ p
                    _    -> do
                        forVars %= (($2, item $1):)
                        return $ voidT :@ p
            else do
                checkDeclVar (None :@ (pos $1)) $2
                err $ BadForVar (item $2) (item $1) (pos $1) (pos $1)
                forVars %= (($2, None):)
                return $ None :@ (pos $1)
    }

ForV -- Or a var
    : VarId
    {% do
        (t :@ p) <- findTypeOfSymbol $1
        if (t `elem` [intT, charT])
            then do
                forVars %= (($1, t):)
                return $ voidT :@ p
            else do
                err $ BadForVar (item $1) t p (pos $1)
                forVars %= (($1, None):)
                return $ None :@ p
    }

Ranges
    : Range
    { $1 }
    | Ranges CLOSE( ";" ) Range
    { checkBoth $1 $3 }

Range
    : Range1 OPEN( "->" ) Insts
    { checkBoth $1 $3 }

Range1
    : from Exp to Exp
    {% checkFor $2 $4 }


---- While loops -----------------------
While
    : while Guards CLOSE( end )
    { item $2 :@ pos $1 }

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

    | Call                          { $1 }

    -- Operators
    ---- Logical
    | Exp and     Exp               {% checkBinOp And      $1 $3 }
    | Exp andalso Exp               {% checkBinOp Andalso  $1 $3 }
    | Exp or      Exp               {% checkBinOp Or       $1 $3 }
    | Exp orelse  Exp               {% checkBinOp Orelse   $1 $3 }
    | Exp xor     Exp               {% checkBinOp Xor      $1 $3 }
    |     not     Exp %prec NEG     {% checkUnOp  Not      (pos $1) $2 }

    ---- Bitwise
    | Exp band Exp                  {% checkBinOp Band     $1 $3 }
    | Exp bor  Exp                  {% checkBinOp Bor      $1 $3 }
    | Exp bsl  Exp                  {% checkBinOp Bsl      $1 $3 }
    | Exp bsr  Exp                  {% checkBinOp Bsr      $1 $3 }
    | Exp bxor Exp                  {% checkBinOp Bxor     $1 $3 }
    |     bnot Exp %prec NEG        {% checkUnOp  Bnot     (pos $1) $2 }

    ---- Arithmetic
    | Exp "+" Exp                   {% checkBinOp Plus     $1 $3 }
    | Exp "-" Exp                   {% checkBinOp Minus    $1 $3 }
    | Exp "*" Exp                   {% checkBinOp Times    $1 $3 }
    | Exp "/" Exp                   {% checkBinOp FloatDiv $1 $3 }
    | Exp div Exp                   {% checkBinOp IntDiv   $1 $3 }
    | Exp rem Exp                   {% checkBinOp Rem      $1 $3 }
    |     "-" Exp %prec NEG         {% checkUnOp  Uminus   (pos $1) $2 }

    ---- Relational
    | Exp "<"  Exp                  {% checkBinOp LTop     $1 $3 }
    | Exp "=<" Exp                  {% checkBinOp LEop     $1 $3 }
    | Exp ">"  Exp                  {% checkBinOp GTop     $1 $3 }
    | Exp ">=" Exp                  {% checkBinOp GEop     $1 $3 }
    | Exp "="  Exp                  {% checkBinOp EQop     $1 $3 }
    | Exp "/=" Exp                  {% checkBinOp NEop     $1 $3 }
    | Exp "|"  Exp                  {% checkBinOp FAop     $1 $3 }
    | Exp "!|" Exp                  {% checkBinOp NFop     $1 $3 }

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
