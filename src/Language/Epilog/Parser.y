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
import           Prelude                        hiding (Either, lookup)
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

    | Either OPEN( ":-" ) Conts CLOSE( "." )
            {% do
                declStruct $1 $3 Either
            }
    | Record OPEN( ":-" ) Conts CLOSE( "." )
            {% do
                declStruct $1 $3 Record
            }


Either 
    : either GenId                  {% do 
                                        current .= Just (pos $2, item $2)
                                        return $2
                                    }
Record 
    : record GenId                  {% do 
                                        current .= Just (pos $2, item $2)
                                        return $2
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
   : Cont                           { [$1]}
   | Conts "," Cont                 {% verifyField $3 $1}

Cont
   : Type VarId                     { ($2, item $1) }


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
    : Type VarId is Exp             {% do 
                                        declVar $1 $2 
                                        if (item $1) /= $4 
                                            then err $ InvalidAssign (item $1) $4 (pos $1)
                                            else return ()
                                    }
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
    : Lval is Exp                   {% do 
                                        symbs <- use symbols
                                        if item $1 /= $3 
                                            then err $ InvalidAssign (item $1) $3 (pos $1)
                                            else return ()
                                    } 

Lval
    : VarId                         { %do 
                                        isSymbol' $1
                                        symbs <- use symbols
                                        case item $1 `lookup` symbs of
                                            Right (Entry _ t _ _) -> return $ (t :@ pos $1)
                                            Left _ -> return (voidT :@ Position (0,0)) 
                                    }



    | Lval "_" VarId                {% do 
                                        isSymbol' $3 

-- - Not working yet - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                        return (voidT :@ Position (0,0)) 
                                    }
    | Lval "{" Exp "]"              {% return (voidT :@ Position (0,0)) }
    | Lval "[" Exp "}"              {% return (voidT :@ Position (0,0)) }

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
    : "(" Exp ")"                   {% return $2      }
    | Bool                          {% return boolT   }
    | Char                          {% return charT   }
    | Int                           {% return intT    }
    | Float                         {% return floatT  }
    | String                        {% return stringT }
    | otherwise                     {% return voidT   }

    | Lval                          {% return boolT }

    | GenId "(" Args ")"            {% do 
                                        symbs <- use symbols
                                        case (item $1 `lookup` symbs) of 
                                            Right (Entry _ t _ _) -> return $ returns t
                                            Left _ -> do
                                                err $ UndefinedProcedure (item $1) (pos $1)
                                                return None
                                    } 

    -- Operators
    ---- Logical
    | Exp and     Exp               {% boolOp $1 $3}
    | Exp andalso Exp               {% boolOp $1 $3}
    | Exp or      Exp               {% boolOp $1 $3}
    | Exp orelse  Exp               {% boolOp $1 $3}
    | Exp xor     Exp               {% boolOp $1 $3}
    |     not     Exp %prec NEG     {% do if $2 == boolT then return boolT else return None }

    ---- Bitwise
    | Exp band Exp                  {% numOp $1 $3}
    | Exp bor  Exp                  {% numOp $1 $3}
    | Exp bsl  Exp                  {% numOp $1 $3}
    | Exp bsr  Exp                  {% numOp $1 $3}
    | Exp bxor Exp                  {% numOp $1 $3}
    |     bnot Exp %prec NEG        {% do if $2 == intT && $2 == floatT then return $2 else return None}

    ---- Array / Record / Either
    | length Exp                    {% return intT }

    ---- Arithmetic
    | Exp "+" Exp                   {% numOp $1 $3}
    | Exp "-" Exp                   {% numOp $1 $3}
    | Exp "*" Exp                   {% numOp $1 $3}
    | Exp "/" Exp                   {% numOp $1 $3}
    | Exp div Exp                   {% numOp $1 $3}
    | Exp rem Exp                   {% numOp $1 $3}
    |     "-" Exp %prec NEG         {% do if $2 == intT && $2 == floatT then return $2 else return None }

    ---- Relational
    | Exp "<"  Exp                  {% numOp $1 $3}
    | Exp "=<" Exp                  {% numOp $1 $3}
    | Exp ">"  Exp                  {% numOp $1 $3}
    | Exp ">=" Exp                  {% numOp $1 $3}
    | Exp "="  Exp                  {% do if $1 == $3 then return $1 else return None}
    | Exp "/=" Exp                  {% do if $1 == $3 then return $1 else return None}
    | Exp "|"  Exp                  {% do if $1 == $3 && $1 == intT then return intT else return None }
    | Exp "!|" Exp                  {% do if $1 == $3 && $1 == intT then return intT else return None }

Bool
    : boolLit                       {} -- { unTokenBoolLit   `fmap` $1 }

Char
    : charLit                       {} -- { unTokenCharLit   `fmap` $1 }

Int
    : intLit                        { unTokenIntLit `fmap` $1 }


Float
    : floatLit                      {} -- { unTokenFloatLit  `fmap` $1 }

String
    : stringLit                     {% string $1 }

{ ------------------------------------------------------------------------------
parseError :: At Token -> Epilog a
parseError (t :@ p) = do
    err $ UnexpectedToken t p
    return undefined
}
