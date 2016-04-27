{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.Token
    ( Token(..)
    , niceShow
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Classes

import           Data.Int                (Int32)
import           Data.List               (intercalate)
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
    | TokenEnd | TokenFor | TokenFrom | TokenTo | TokenIf | TokenOtherwise
    | TokenWhile

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
    | TokenUnderscore

    -- Assignment
    | TokenIs

    -- IO
    |  TokenRead | TokenWrite

    -- Literals
    | TokenCharacterLiteral { unTokenCharacterLiteral :: Char }
    | TokenFloatLiteral     { unTokenFloatLiteral :: Float }
    | TokenIntegerLiteral   { unTokenIntegerLiteral :: Int32 }
    | TokenBooleanLiteral   { unTokenBoolLiteral :: Bool }
    | TokenStringLiteral    { unTokenStringLiteral :: String }

        -- Identifier
    | TokenVariableIdentifier { unTokenVariableIdentifier :: String }
    | TokenGeneralIdentifier  { unTokenGeneralIdentifier :: String }

    -- Error
    | ErrorUnderflow { unErrorUnderflow :: Integer }
    | ErrorOverflow { unErrorOverflow :: Integer }
    | ErrorUnclosedStringLiteral { unErrorUnclosedStringLiteral :: String }
    | ErrorUnexpectedToken { unUnexpectedToken :: Char }

    -- EOF
    | TokenEOF {- Temporal, no será necesario con el Parser -}
    deriving (Eq, Show, Read)

instance NiceShow Token where
    niceShow = \case

    -- Logical Operators
        TokenAnd     -> "TOKEN: and"
        TokenAndalso -> "TOKEN: andalso"
        TokenOr      -> "TOKEN: or"
        TokenOrelse  -> "TOKEN: orelse"
        TokenNot     -> "TOKEN: not"

    -- Bitwise Operations
        TokenBand -> "TOKEN: band"
        TokenBnot -> "TOKEN: bnot"
        TokenBor  -> "TOKEN: bor"
        TokenBsl  -> "TOKEN: bsl"
        TokenBsr  -> "TOKEN: bsr"
        TokenBxor -> "TOKEN: bxor"

    -- Array
        TokenLength -> "TOKEN: length"
        TokenColon  -> "TOKEN: :"

    -- Arithmetic Operators
        TokenPlus            -> "TOKEN: +"
        TokenMinus           -> "TOKEN: -"
        TokenTimes           -> "TOKEN: *"
        TokenFloatDivision   -> "TOKEN: /"
        TokenIntegerDivision -> "TOKEN: div"
        TokenRem             -> "TOKEN: rem"

    -- Relational
        TokenLT       -> "TOKEN: <"
        TokenLTE      -> "TOKEN: =<"
        TokenGT       -> "TOKEN: >"
        TokenGTE      -> "TOKEN: >="
        TokenFactorOf -> "TOKEN: |"

    -- Equality
        TokenEQ -> "TOKEN: ="
        TokenNE -> "TOKEN: /="

    -- Control Structures
        TokenEnd       -> "TOKEN: end"
        TokenFor       -> "TOKEN: for"
        TokenFrom      -> "TOKEN: from"
        TokenTo        -> "TOKEN: to"
        TokenIf        -> "TOKEN: if"
        TokenOtherwise -> "TOKEN: otherwise"
        TokenWhile     -> "TOKEN: while"

    -- Functions and Procedures
        TokenFinish    -> "TOKEN: finish"
        TokenFunction  -> "TOKEN: function"
        TokenProcedure -> "TOKEN: procedure"
        TokenReturn    -> "TOKEN: return"
        TokenDefine    -> "TOKEN: :-"

    -- Composite Types
        TokenEither -> "TOKEN: either"
        TokenRecord -> "TOKEN: record"

    -- Conversion
        TokenToBoolean   -> "TOKEN: toBoolean"
        TokenToCharacter -> "TOKEN: toCharacter"
        TokenToFloat     -> "TOKEN: toFloat"
        TokenToInteger   -> "TOKEN: toInteger"

    -- Types
        TokenBooleanType   -> "TOKEN: boolean"
        TokenCharacterType -> "TOKEN: character"
        TokenFloatType     -> "TOKEN: float"
        TokenIntegerType   -> "TOKEN: integer"
        TokenStringType    -> "TOKEN: string"
        TokenVoidType      -> "TOKEN: void"

    -- Punctuation
        TokenComma            -> "TOKEN: ,"
        TokenPeriod           -> "TOKEN: ."
        TokenSemicolon        -> "TOKEN: ;"
        TokenArrow            -> "TOKEN: ->"
        TokenLeftParenthesis  -> "TOKEN: ("
        TokenRightParenthesis -> "TOKEN: )"
        TokenUnderscore       -> "TOKEN: _"

    -- Assign
        TokenIs -> "TOKEN: is"

    -- IO
        TokenRead  -> "TOKEN: read"
        TokenWrite -> "TOKEN: write"

    -- Literals
        TokenCharacterLiteral value ->
            intercalate "\n"
                [ "TOKEN: Character Literal"
                , "VALUE: " ++ (tail . init . show $ value)
                ]
        TokenFloatLiteral value ->
            intercalate "\n"
                [ "TOKEN: Float Literal"
                , "VALUE: " ++ show value
                ]
        TokenIntegerLiteral value ->
            intercalate "\n"
                [ "TOKEN: Integer Literal"
                , "VALUE: " ++ show value
                ]
        TokenBooleanLiteral value ->
            intercalate "\n"
                [ "TOKEN: Boolean Literal"
                , "VALUE: " ++ (if value then "true" else "false")
                ]
        TokenStringLiteral value ->
            intercalate "\n"
                [ "TOKEN: String Literal"
                , "VALUE: " ++ (tail . init . show $ value)
                ]

    -- Identifier
        TokenVariableIdentifier name ->
            intercalate "\n"
                [ "TOKEN: Variable ID"
                , "VALUE: " ++ name
                ]
        TokenGeneralIdentifier name ->
            intercalate "\n"
                [ "TOKEN: General ID"
                , "VALUE: " ++ name
                ]

    -- Error
        ErrorUnderflow value ->
            intercalate "\n"
                [ "ERROR: Underflow"
                , "VALUE: " ++ show value
                ]
        ErrorOverflow value ->
            intercalate "\n"
                [ "ERROR: Overflow"
                , "VALUE: " ++ show value
                ]
        ErrorUnclosedStringLiteral value ->
            intercalate "\n"
                [ "ERROR: Unclosed String Literal"
                , "VALUE: " ++ value
                ]
        ErrorUnexpectedToken value ->
            intercalate "\n"
                [ "ERROR: Unexpected Token"
                , "VALUE: " ++ [value]
                ]

    -- EOF
        TokenEOF -> "TOKEN: EOF"
