{
module Language.Epilog.Parser
    ( parseProgram
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Type
import           Language.Epilog.At
import           Language.Epilog.Lexer
import           Language.Epilog.Context 

--------------------------------------------------------------------------------
import           Data.Int                        (Int32)
import           Data.Sequence                   (Seq, (<|), (><), (|>), ViewL((:<)))
import qualified Data.Sequence                   as Seq (empty, singleton, viewl)
import           Prelude                         hiding (Either)
import           Control.Monad.Trans.RWS.Strict  (RWS, execRWS, get, gets,
                                                  modify, tell)

--------------------------------------------------------------------------------
}

%name parser
%tokentype { At Token }
%monad { Context }
--%monad { Alex }
--%lexer { lexer } { EOF :@ _ }
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
--Program :: { Program }
    --: TopDefs                       { Program $1 }

-- Top Level Definitions --------------
TopDefs :: { () }
    : TopDef                        { % return () }
    | TopDefs TopDef                { % return () }

TopDef :: {  () }
    --: proc GenId "(" Params0 ")" ":-" Insts "."
    --                                { % Return () }
--    | proc GenId "(" Params0 ")" "->" Type ":-" Insts "."
--                                    { ProcD   (pos $1) (item $2) $4 (item $7) $9 }
--    | either GenId ":-" Conts "."   { StructD (pos $1) (item $2) Either $4 }
--    | record GenId ":-" Conts "."   { StructD (pos $1) (item $2) Record $4 }
      : Declaration "."             { % return () }
      | Initialization "."          { % return () }

GenId :: { At String }
    : genId                         { unTokenGenId `fmap` $1}

--Params0 :: { Params }
--    : {- lambda -}                  { Seq.empty }
--    | Params                        { $1 }

--Params :: { Params }
--    : Param                         { Seq.singleton $1 }
--    | Params "," Param              { $1 |> $3 }

--Param :: { Parameter }
--    : Type VarId                    { Parameter (pos $1) (item $1) (item $2) }

--Conts :: { Conts }
--    : Cont                          { Seq.singleton $1 }
--    | Conts "," Cont                { $1 |> $3 }

--Cont :: { Content }
--    : Type VarId                    { Content (pos $1) (item $1) (item $2) }

---- Instructions ------------------------
Insts :: { () }
    : Inst                          { % return () }
    | Insts "," Inst                { % return () }

Inst :: { () }
    : Declaration                   { % return () }
    | Initialization                { % return () }
    | Assign                        { % return () }
--    | Call                          { $1 }
--    | If                            { $1 }
--    | Case                          { $1 }
--    | For                           { $1 }
--    | While                         { $1 }
--    | read Lval                     { Read   (pos $1) (item $2) }
--    | write Exp                     { Write  (pos $1) $2 }
--    | finish                        { Finish (pos $1) }

------ Declaration and Initialization ----
Declaration :: { () }
    : Type VarId                    { % do inst (Declaration (pos $1) (item $1) (item $2) Nothing) }

Initialization :: { () }
    : Type VarId is Exp             { % do 
                                        expr <- gets expression
                                        case Seq.viewl expr of 
                                            x :< xs ->
                                                inst (Declaration (pos $1) (item $1) (item $2) (Just x)) }

Type :: { At Type }
    : GenId                         { Type (item $1) Seq.empty <$ $1 }
    --| GenId ArraySize               { Type (item $1) $2 <$ $1 }

--ArraySize :: { Seq Int32 }
--    : "{" Int "]"                   { Seq.singleton (item $2) }
--    | "[" Int "}"                   { Seq.singleton (item $2) }
--    | ArraySize "{" Int "]"         { $1 |> (item $3) }
--    | ArraySize "[" Int "}"         { $1 |> (item $3) }

------ Assignment ------------------------
Assign :: { () }
    : Lval is Exp                   { % do 
                                        expr <- gets expression
                                        case Seq.viewl expr of 
                                            x :< xs ->
                                                inst $ Assign (pos $1) (item $1) x }

Lval :: { At Lval }
    : VarId                         { Variable (item $1)           <$ $1 }
--    | Lval "_" VarId                { Member   (item $1) (item $3) <$ $1 }
--    | Lval "{" Exp "]"              { Index    (item $1)       $3  <$ $1 }
--    | Lval "[" Exp "}"              { Index    (item $1)       $3  <$ $1 }

VarId :: { At String }
    : varId                         { unTokenVarId `fmap` $1 }

------ Call ------------------------------
--Call :: { Instruction }
--    : GenId "(" Args ")"            { ICall (pos $1) (item $1) $3 }

--Args :: { Exps }
--    : {- lambda -}                  { Seq.empty }
--    | Args1                         { $1 }

--Args1 :: { Exps }
--    : Exp                           { Seq.singleton $1 }
--    | Args1 "," Exp                 { $1 |> $3 }

------ If --------------------------------
--If :: { Instruction }
--    : if Guards end                 { If (pos $1) $2}

--Guards :: { Guards }
--    : Guard                         { Seq.singleton $1 }
--    | Guards ";" Guard              { $1 |> $3 }

--Guard :: { Guard }
--    : Exp "->" Insts                { (pos $1, $1, $3) }

------ Case ------------------------------
--Case :: { Instruction }
--    : case Exp of Sets end          { Case (pos $1) $2 $4 }

--Sets :: { Sets }
--    : Set                           { Seq.singleton $1 }
--    | Sets ";" Set                  { $1 |> $3 }

--Elems :: { At Exps }
--    : Exp                           { (Seq.singleton $1) :@ (pos $1) }
--    | Elems "," Exp                 { ((item $1) |> $3) :@ (pos $1) }

--Set :: { Set }
--    : Elems "->" Insts              { (pos $1, item $1, $3) }

------ For loops -------------------------
--For :: { Instruction }
--    : for      VarId Ranges end     { For (pos $1)  Nothing         (item $2) $3 }
--    | for Type VarId Ranges end     { For (pos $1) (Just (item $2)) (item $3) $4 }

--Ranges :: { Ranges }
--    : Range                         { Seq.singleton $1 }
--    | Ranges ";" Range              { $1 |> $3 }

--Range :: { Range }
--    : from Exp to Exp "->" Insts    { (pos $1, $2, $4, $6) }

------ While loops -----------------------
--While :: { Instruction }
--    : while Guards end              { While (pos $1) $2 }

---- Expressions -------------------------
Exp :: { () }
    : "(" Exp ")"                   { % modify (\s -> s)  }
    | Bool                          { % literal (LitBool   (pos $1) (item $1)) }
    | Char                          { % literal (LitChar   (pos $1) (item $1)) }
    | Int                           { % literal (LitInt    (pos $1) (item $1)) }
    | Float                         { % literal (LitFloat  (pos $1) (item $1)) }
    | String                        { % literal (LitString (pos $1) (item $1)) }
--    | otherwise                     { Otherwise (pos $1) }

    | Lval                          { % do
                                        let lval = Lval (pos $1) (item $1)
                                        modify (\s -> s {expression = lval <| expression s})}

--    | GenId "(" Args ")"            { ECall (pos $1) (item $1) $3 }

--    -- Operators
    -- Logical
    | Exp and     Exp               { % binaryOperation And    }
    | Exp andalso Exp               { % binaryOperation Andalso}
    | Exp or      Exp               { % binaryOperation Or     }
    | Exp orelse  Exp               { % binaryOperation Orelse }
    | Exp xor     Exp               { % binaryOperation Xor    }
    |     not     Exp %prec NEG     { % unaryOperation  Not }

    ---- Bitwise
    | Exp band Exp                  { % binaryOperation Band }
    | Exp bor  Exp                  { % binaryOperation Bor  }
    | Exp bsl  Exp                  { % binaryOperation Bsl  }
    | Exp bsr  Exp                  { % binaryOperation Bsr  }
    | Exp bxor Exp                  { % binaryOperation Bxor }
    |     bnot Exp %prec NEG        { % unaryOperation  Bnot }

--    ---- Array / Record / Either
    | length Exp                    { % return () }

----    ---- Arithmetic
    | Exp "+" Exp                   { % binaryOperation Plus   }
    | Exp "-" Exp                   { % binaryOperation Minus  }
    | Exp "*" Exp                   { % binaryOperation Times  }
    | Exp "/" Exp                   { % binaryOperation FloatDiv }
    | Exp div Exp                   { % binaryOperation IntDiv }
    | Exp rem Exp                   { % binaryOperation Rem    }
    |     "-" Exp %prec NEG         { % unaryOperation Uminus }

    ---- Relational
    | Exp "<"  Exp                  { % binaryOperation LTop }
    | Exp "=<" Exp                  { % binaryOperation LEop }
    | Exp ">"  Exp                  { % binaryOperation GTop }
    | Exp ">=" Exp                  { % binaryOperation GEop }
    | Exp "="  Exp                  { % binaryOperation EQop }
    | Exp "/=" Exp                  { % binaryOperation NEop }
    | Exp "|"  Exp                  { % binaryOperation FAop }
    | Exp "!|" Exp                  { % binaryOperation NFop }

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


--lexer :: (At Token -> Context ()) -> Context ()
--lexer cont = do
--    let l@(t :@ _) = alexMonadScan
--    case t of
--        ErrorUnderflow _ -> do
--            lexer cont
--        ErrorOverflow _ -> do
--            lexer cont
--        ErrorUnclosedStringLit s -> do
--            cont $ TokenStringLit s <$ l
--        ErrorUnexpectedToken _ -> do
--            lexer cont
--        _ -> cont l

--parseError :: [Lexeme Token] -> a
parseError l = case l of
  [] -> error $ "Unexpected EOF"
  _  -> error $ "Unexpected " ++ show (head l)

--parseProgram :: String -> (Program, String)
parseProgram input = 
    case scanner input of
            Left  msg    -> error msg
            Right tokens ->  execRWS (parser tokens) () initialState

                    


}
