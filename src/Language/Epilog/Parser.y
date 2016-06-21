{ module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import qualified Language.Epilog.AST.AST         as AST
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
    : PREPARE OPEN TopDefs CLOSE     {}

PREPARE
    : {- lambda -}
    {% prepare }

OPEN
    : {- lambda -}
    {% symbols %= openScope (Position (1,1)) }

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
        offset %= (0:)
        structSize  .= 0
        structAlign .= 0
    }
StructKind
    : either { EitherK :@ (pos $1) }
    | record { RecordK :@ (pos $1) }

Procedure
    : Procedure1 Procedure2 Procedure3
    {%

        storeProcedure' $3
    }
Procedure1
    : proc GenId OPENF( "(" ) Params0 ")"
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
    : OPENF( ":-" ) Insts CLOSE(CLOSE( "." ))
    { $2 }


OPENF(TOKEN)
    : TOKEN
    {% do
        symbols %= openScope (pos $1)
        offset %= (0:)
        AST.openScope
        return $1
    }

OPEN(TOKEN)
    : TOKEN
    {% do
        symbols %= openScope (pos $1)
        AST.openScope
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
    {% checkDeclVar $1 $2}

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
    { checkBoth $1 $3 }

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
    {% checkAnswer voidT (pos $1) }

    | answer Exp
    {% checkAnswer (item $2) (pos $1) }

    | write Exp
    {% checkWrite (item $2) (pos $1) }

    | read Lval
    {% checkRead (item $2) (pos $1) }

------ Declaration and Initialization ----
Declaration
    : Type VarId
    {% do
        AST.insertInst $ (Declaration (pos $1) (item $1) (item $2) Nothing)
        checkDeclVar $1 $2
    }

Initialization
    : Type VarId is Exp
    {% do
        init <- AST.topExpr
        AST.insertInst $ (Declaration (pos $1) (item $1) (item $2) (Just init))
        checkInit $1 $2 $4 }

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
    {% buildPointers (item $1) (item $2) :@ (pos $1) }

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
    {% do
        AST.assign
        checkAssign $1 $3
    }


Lval
    : VarId
    {% do
        AST.insertLval $ Lval (pos $1) (Variable (item $1))
        isSymbol' $1
        findTypeOfSymbol $1
    }

    | Lval "[" Exp "]"
    {% do
        expr <- AST.topExpr
        (Lval p lval) <- AST.topLval
        AST.insertLval $ Lval p (Index lval expr)
        checkArray $1 (item $3)
    }

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
    {% do
        AST.buildIf (pos $1)
        return $ item $2 :@ pos $1
    }

Guards
    : Guard
    { $1 }
    | Guards CLOSE( ";" ) Guard
    { checkBoth $1 $3 }

Guard
    : GuardCond OPEN( "->" ) Insts
    {% do
        AST.guard (pos $1)
        return $ checkBoth $1 $3
    }

GuardCond
    : Exp
    {%
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
        AST.buildCase (pos $1)
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
    {% do
        AST.caseSets (pos $1)
        return $ checkBoth $1 $3
    }

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
            then do
                caseSet %= (|> LitInt (pos $1) (item $1))
                return $ intT :@ pos $1
            else do
                err $ BadCaseCharElem p (item $1) (pos $1)
                return $ None :@ (pos $1)
    }
    | Char
    {% do
        ((ct :@ p):_) <- use caseTypes
        if (ct == charT)
            then do
                caseSet %= (|> LitChar (pos $1) (item $1))
                return $ charT :@ pos $1
            else do
                err $ BadCaseIntElem p (item $1) (pos $1)
                return $ None :@ (pos $1)
    }


---- For loops -------------------------
For
    :       for   ForV Ranges CLOSE( end )
    {% do
        AST.buildFor (pos $1) False
        forVars %= tail
        return $ checkBoth $2 $3
    }

    | OPEN( for ) ForD Ranges CLOSE( CLOSE( end ) )
    {% do
        i <- AST.topInsts
        AST.buildFor (pos $1) True

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
    {% do
        AST.range (pos $1)
        return $ checkBoth $1 $3
    }

Range1
    : from Exp to Exp
    {% checkFor $2 $4 }


---- While loops -----------------------
While
    : while Guards CLOSE( end )
    {% do
        AST.buildWhile (pos $1)
        return $ item $2 :@ pos $1
    }

---- Expressions -------------------------
Exp
    : "(" Exp ")"                   { $2 }
    | Bool                          {% do
                                        AST.insertExpr $ LitBool (pos $1) (item $1)
                                        return $ boolT :@ pos $1
                                    }
    | Char                          {% do
                                        AST.insertExpr $ LitChar (pos $1) (item $1)
                                        return $ charT :@ pos $1
                                    }
    | Int                           {% do
                                        AST.insertExpr $ LitInt (pos $1) (item $1)
                                        return $ intT  :@ pos $1
                                    }
    | Float                         {% do
                                        AST.insertExpr $ LitFloat (pos $1) (item $1)
                                        return $ floatT :@ pos $1
                                    }
    | String                        {% do
                                        AST.insertExpr $ LitString (pos $1) (item $1)
                                        return $ stringT :@ pos $1
                                    }
    -- | otherwise                  { voidT   }

    | Lval                          {% do
                                        lval <- AST.topLval
                                        AST.insertExpr lval
                                        return $1
                                    }

    | Call                          { $1 }

    -- Operators
    ---- Logical
    | Exp and     Exp               {% AST.binOp And     (pos $2) >> checkBinOp And     (pos $2) $1 $3 }
    | Exp andalso Exp               {% AST.binOp Andalso (pos $2) >> checkBinOp Andalso (pos $2) $1 $3 }
    | Exp or      Exp               {% AST.binOp Or      (pos $2) >> checkBinOp Or      (pos $2) $1 $3 }
    | Exp orelse  Exp               {% AST.binOp Orelse  (pos $2) >> checkBinOp Orelse  (pos $2) $1 $3 }
    | Exp xor     Exp               {% AST.binOp Xor     (pos $2) >> checkBinOp Xor     (pos $2) $1 $3 }
    |     not     Exp %prec NEG     {% checkUnOp Not     (pos $1) $2 }

    ---- Bitwise
    | Exp band Exp                  {% AST.binOp Band (pos $2) >> checkBinOp Band (pos $2) $1 $3 }
    | Exp bor  Exp                  {% AST.binOp Bor  (pos $2) >> checkBinOp Bor  (pos $2) $1 $3 }
    | Exp bsl  Exp                  {% AST.binOp Bsl  (pos $2) >> checkBinOp Bsl  (pos $2) $1 $3 }
    | Exp bsr  Exp                  {% AST.binOp Bsr  (pos $2) >> checkBinOp Bsr  (pos $2) $1 $3 }
    | Exp bxor Exp                  {% AST.binOp Bxor (pos $2) >> checkBinOp Bxor (pos $2) $1 $3 }
    |     bnot Exp %prec NEG        {% checkUnOp Bnot (pos $1) $2 }

    ---- Arithmetic
    | Exp "+" Exp                   {% AST.binOp Plus     (pos $2) >> checkBinOp Plus     (pos $2) $1 $3 }
    | Exp "-" Exp                   {% AST.binOp Minus    (pos $2) >> checkBinOp Minus    (pos $2) $1 $3 }
    | Exp "*" Exp                   {% AST.binOp Times    (pos $2) >> checkBinOp Times    (pos $2) $1 $3 }
    | Exp "/" Exp                   {% AST.binOp FloatDiv (pos $2) >> checkBinOp FloatDiv (pos $2) $1 $3 }
    | Exp div Exp                   {% AST.binOp IntDiv   (pos $2) >> checkBinOp IntDiv   (pos $2) $1 $3 }
    | Exp rem Exp                   {% AST.binOp Rem      (pos $2) >> checkBinOp Rem      (pos $2) $1 $3 }
    |     "-" Exp %prec NEG         {% checkUnOp Uminus   (pos $1) $2 }

    ---- Relational
    | Exp "<"  Exp                  {% AST.binOp LTop (pos $2) >> checkBinOp LTop (pos $2) $1 $3 }
    | Exp "=<" Exp                  {% AST.binOp LEop (pos $2) >> checkBinOp LEop (pos $2) $1 $3 }
    | Exp ">"  Exp                  {% AST.binOp GTop (pos $2) >> checkBinOp GTop (pos $2) $1 $3 }
    | Exp ">=" Exp                  {% AST.binOp GEop (pos $2) >> checkBinOp GEop (pos $2) $1 $3 }
    | Exp "="  Exp                  {% AST.binOp EQop (pos $2) >> checkBinOp EQop (pos $2) $1 $3 }
    | Exp "/=" Exp                  {% AST.binOp NEop (pos $2) >> checkBinOp NEop (pos $2) $1 $3 }
    | Exp "|"  Exp                  {% AST.binOp FAop (pos $2) >> checkBinOp FAop (pos $2) $1 $3 }
    | Exp "!|" Exp                  {% AST.binOp NFop (pos $2) >> checkBinOp NFop (pos $2) $1 $3 }

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
