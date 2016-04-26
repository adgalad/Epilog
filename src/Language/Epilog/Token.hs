{-# LANGUAGE LambdaCase #-}

module Language.Epilog.Token
    ( Token(..)
    ) where
--------------------------------------------------------------------------------
import           Data.Int (Int32)
--------------------------------------------------------------------------------

data Token

    -- Logical Operators
    = TokenAnd | TokenAndalso | TokenOr | TokenOrelse | TokenNot

    -- Bitwise Operators
    | TokenBand | TokenBnot | TokenBor | TokenBsl  | TokenBsr  | TokenBxor

    -- Array
    | TokenLength | TokenColon

    -- Arithmetic Operators
    | TokenPlus | TokenMinus | TokenTimes | TokenFloatDivision | TokenRem
    | TokenIntegerDivision

    -- Relational
    | TokenLT | TokenLTE | TokenGT | TokenGTE | TokenFactorOf

    -- Equality
    | TokenEQ | TokenNE

    -- Control Structures
    | TokenEnd | TokenFor | TokenIf | TokenOtherwise | TokenWhile

    -- Functions and Procedures
    | TokenFinish | TokenFunction | TokenProcedure | TokenReturn
    | TokenDefine

    -- Composite Types
    | TokenEither | TokenRecord

    -- Conversion
    | TokenToBoolean | TokenToCharacter | TokenToFloat | TokenToInteger

    -- Types
    | TokenBooleanType | TokenCharacterType | TokenFloatType
    | TokenIntegerType | TokenStringType    | TokenVoidType

    -- Punctuation
    | TokenComma      | TokenPeriod          | TokenSemicolon
    | TokenArrow      | TokenLeftParenthesis | TokenRightParenthesis
    | TokenUnderscore | TokenLeftCurly       | TokenRightCurly

    -- Assignment
    | TokenIs

    -- IO
    |  TokenRead | TokenWrite

    -- Identifier
    | TokenVariableIdentifier { unTokenVariableIdentifier :: String }
    | TokenGeneralIdentifier  { unTokenGeneralIdentifier :: String }

    -- Literals
    | TokenCharacterLiteral { unTokenCharacterLiteral :: Char }
    | TokenFloatLiteral     { unTokenFloatLiteral :: Float }
    | TokenIntegerLiteral   { unTokenIntegerLiteral :: Int32 }
    | TokenBooleanLiteral   { unTokenBoolLiteral :: Bool }
    | TokenStringLiteral    { unTokenStringLiteral :: String }

    -- Error
    | ErrorUnderflow { unErrorUnderflow :: Integer }
    | ErrorOverflow { unErrorOverflow :: Integer }
    | ErrorUnclosedStringLiteral { unErrorUnclosedStringLiteral :: String }

    -- EOF
    | TokenEOF {- Temporal, no será necesario con el Parser -}
    deriving (Eq)

instance Show Token where
    show = \case

    -- Logical Operators
        TokenAnd     -> "Token AND"
        TokenAndalso -> "Token ANDALSO"
        TokenOr      -> "Token OR"
        TokenOrelse  -> "Token ORELSE"
        TokenNot     -> "Token NOT"

    -- Bitwise Operations
        TokenBand -> "Token BAND"
        TokenBnot -> "Token BNOT"
        TokenBor  -> "Token BOR"
        TokenBsl  -> "Token BSL"
        TokenBsr  -> "Token BSR"
        TokenBxor -> "Token BXOR"

    -- Array
        TokenLength -> "Token LENGTH"
        TokenColon  -> "Token :"

    -- Arithmetic Operators
        TokenPlus            -> "Token +"
        TokenMinus           -> "Token -"
        TokenTimes           -> "Token *"
        TokenFloatDivision   -> "Token /"
        TokenIntegerDivision -> "Token DIV"
        TokenRem             -> "Token REM"

    -- Relational
        TokenLT       -> "Token <"
        TokenLTE      -> "Token =<"
        TokenGT       -> "Token >"
        TokenGTE      -> "Token >="
        TokenFactorOf -> "Token |"

    -- Equality
        TokenEQ -> "Token ="
        TokenNE -> "Token /="

    -- Control Structures
        TokenEnd       -> "Token END"
        TokenFor       -> "Token FOR"
        TokenIf        -> "Token IF"
        TokenOtherwise -> "Token OTHERWISE"
        TokenWhile     -> "Token WHILE"

    -- Functions and Procedures
        TokenFinish    -> "Token FINISH"
        TokenFunction  -> "Token FUNCTION"
        TokenProcedure -> "Token PROCEDURE"
        TokenReturn    -> "Token RETURN"
        TokenDefine    -> "Token :-"

    -- Composite Types
        TokenEither -> "Token EITHER"
        TokenRecord -> "Token RECORD"

    -- Conversion
        TokenToBoolean   -> "Token TOBOOLEAN"
        TokenToCharacter -> "Token TOCHARACTER"
        TokenToFloat     -> "Token TOFLOAT"
        TokenToInteger   -> "Token TOINTEGER"

    -- Types
        TokenBooleanType   -> "Token BOOLEAN"
        TokenCharacterType -> "Token CHARACTER"
        TokenFloatType     -> "Token FLOAT"
        TokenIntegerType   -> "Token INTEGER"
        TokenStringType    -> "Token STRING"
        TokenVoidType      -> "Token VOID"

    -- Identifier
        TokenVariableIdentifier name ->
            "Token VARID (" ++ name ++ ")"
        TokenGeneralIdentifier name ->
            "Token GENERALID (" ++ name ++ ")"

    -- Punctuation
        TokenComma            -> "Token ,"
        TokenPeriod           -> "Token ."
        TokenSemicolon        -> "Token ;"
        TokenArrow            -> "Token ->"
        TokenLeftParenthesis  -> "Token ("
        TokenRightParenthesis -> "Token )"
        TokenUnderscore       -> "Token _"
        TokenLeftCurly        -> "Token {"
        TokenRightCurly       -> "Token }"

    -- Literals
        TokenCharacterLiteral value ->
            "Token CHARACTER (" ++ show value ++ ")"
        TokenFloatLiteral value ->
            "Token FLOAT (" ++ show value ++ ")"
        TokenIntegerLiteral value ->
            "Token INTEGER (" ++ show value ++ ")"
        TokenBooleanLiteral value ->
            "Token BOOLEAN (" ++ show value ++ ")"
        TokenStringLiteral value ->
            "Token STRING (" ++ show value ++ ")"

    -- Assign
        TokenIs -> "Token IS"

    -- IO
        TokenRead  -> "Token READ"
        TokenWrite -> "Token WRITE"

    -- Error
        ErrorUnderflow value ->
            "ERROR UNDERFLOW (" ++ show value ++ ")"
        ErrorOverflow value ->
            "ERROR OVERFLOW (" ++ show value ++ ")"
        ErrorUnclosedStringLiteral value ->
            "ERROR UNCLOSED STRING LITERAL (" ++ show value ++ ")"

    -- EOF
        TokenEOF -> "Token EOF"
