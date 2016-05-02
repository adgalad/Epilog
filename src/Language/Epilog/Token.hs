{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.Token
    ( Token(..)
    , isError
    ) where
--------------------------------------------------------------------------------
import           Data.Char               (showLitChar)
import           Data.Int                (Int32)
import           Data.List               (intercalate)
import           GHC.Show                (showLitString)
--------------------------------------------------------------------------------

data Token
    -- Operators
    ---- Logical
    = TokenAnd | TokenAndalso | TokenOr | TokenOrelse | TokenXor | TokenNot

    ---- Bitwise
    | TokenBand | TokenBor | TokenBnot | TokenBsl  | TokenBsr  | TokenBxor

    ---- Array / Record / Either
    | TokenLength | TokenColon | TokenUnderscore

    ---- Arithmetic
    | TokenPlus | TokenMinus | TokenTimes | TokenFloatDiv
    | TokenIntDiv | TokenRem

    ---- Relational
    | TokenLT | TokenLE | TokenGT | TokenGE
    | TokenEQ | TokenNE
    | TokenFA | TokenNF

    -- Control Structures
    | TokenEnd | TokenFor | TokenFrom | TokenTo | TokenIf | TokenOtherwise
    | TokenWhile | TokenCase | TokenOf

    -- Functions and Procedures
    | TokenFinish | TokenFunction | TokenProcedure | TokenReturn
    | TokenDefine

    -- Composite Types
    | TokenEither | TokenRecord

    -- Global Declaration
    | TokenGlobal

    -- Conversion
    | TokenToBool | TokenToChar | TokenToInt | TokenToFloat

    -- Types
    | TokenVoidType | TokenBoolType  | TokenCharType
    | TokenIntType  | TokenFloatType | TokenStringType

    -- Punctuation
    | TokenComma | TokenPeriod  | TokenSemicolon
    | TokenArrow | TokenLeftPar | TokenRightPar

    -- Assignment
    | TokenIs

    -- IO
    |  TokenRead | TokenWrite

    -- Literals
    | TokenBoolLit   { unTokenBoolLit   :: Bool   }
    | TokenCharLit   { unTokenCharLit   :: Char   }
    | TokenIntLit    { unTokenIntLit    :: Int32  }
    | TokenFloatLit  { unTokenFloatLit  :: Float  }
    | TokenStringLit { unTokenStringLit :: String }

    -- Identifier
    | TokenVarId { unTokenVarId :: String }
    | TokenGenId { unTokenGenId :: String }

    -- Error
    | ErrorUnderflow         { unErrorUnderflow :: String }
    | ErrorOverflow          { unErrorOverflow :: String }
    | ErrorUnclosedStringLit { unErrorUnclosedStringLit :: String }
    | ErrorUnexpectedToken   { unUnexpectedToken :: Char }

    -- EOF
    | EOF
    deriving (Eq)

isError :: Token -> Bool
isError (ErrorUnderflow             _) = True
isError (ErrorOverflow              _) = True
isError (ErrorUnclosedStringLit _) = True
isError (ErrorUnexpectedToken       _) = True
isError _                              = False

instance Show Token where
    show = \case

    -- Operators
    ---- Logical
        TokenAnd     -> "TOKEN: and"
        TokenAndalso -> "TOKEN: andalso"
        TokenOr      -> "TOKEN: or"
        TokenOrelse  -> "TOKEN: orelse"
        TokenXor     -> "TOKEN: xor"
        TokenNot     -> "TOKEN: not"

    ---- Bitwise
        TokenBand -> "TOKEN: band"
        TokenBor  -> "TOKEN: bor"
        TokenBnot -> "TOKEN: bnot"
        TokenBsl  -> "TOKEN: bsl"
        TokenBsr  -> "TOKEN: bsr"
        TokenBxor -> "TOKEN: bxor"

    ---- Array
        TokenLength -> "TOKEN: length"
        TokenColon  -> "TOKEN: :"
        TokenUnderscore -> "TOKEN: _"

    ---- Arithmetic
        TokenPlus     -> "TOKEN: +"
        TokenMinus    -> "TOKEN: -"
        TokenTimes    -> "TOKEN: *"
        TokenFloatDiv -> "TOKEN: /"
        TokenIntDiv   -> "TOKEN: div"
        TokenRem      -> "TOKEN: rem"

    ---- Relational
        TokenLT        -> "TOKEN: <"
        TokenLE       -> "TOKEN: =<"
        TokenGT        -> "TOKEN: >"
        TokenGE       -> "TOKEN: >="
        TokenEQ        -> "TOKEN: ="
        TokenNE        -> "TOKEN: /="
        TokenFA    -> "TOKEN: |"
        TokenNF -> "TOKEN: !|"

    -- Control Structures
        TokenEnd       -> "TOKEN: end"
        TokenFor       -> "TOKEN: for"
        TokenFrom      -> "TOKEN: from"
        TokenTo        -> "TOKEN: to"
        TokenIf        -> "TOKEN: if"
        TokenOtherwise -> "TOKEN: otherwise"
        TokenWhile     -> "TOKEN: while"
        TokenCase      -> "TOKEN: case"
        TokenOf        -> "TOKEN: of"

    -- Functions and Procedures
        TokenFinish    -> "TOKEN: finish"
        TokenFunction  -> "TOKEN: function"
        TokenProcedure -> "TOKEN: procedure"
        TokenReturn    -> "TOKEN: return"
        TokenDefine    -> "TOKEN: :-"

    -- Composite Types
        TokenEither -> "TOKEN: either"
        TokenRecord -> "TOKEN: record"

    -- Global Declaration
        TokenGlobal -> "TOKEN: global"

    -- Conversion
        TokenToBool  -> "TOKEN: toBoolean"
        TokenToChar  -> "TOKEN: toCharacter"
        TokenToFloat -> "TOKEN: toFloat"
        TokenToInt   -> "TOKEN: toInteger"

    -- Types
        TokenVoidType   -> "TOKEN: void"
        TokenBoolType   -> "TOKEN: boolean"
        TokenCharType   -> "TOKEN: character"
        TokenIntType    -> "TOKEN: integer"
        TokenFloatType  -> "TOKEN: float"
        TokenStringType -> "TOKEN: string"

    -- Punctuation
        TokenComma      -> "TOKEN: ,"
        TokenPeriod     -> "TOKEN: ."
        TokenSemicolon  -> "TOKEN: ;"
        TokenArrow      -> "TOKEN: ->"
        TokenLeftPar    -> "TOKEN: ("
        TokenRightPar   -> "TOKEN: )"

    -- Assign
        TokenIs -> "TOKEN: is"

    -- IO
        TokenRead  -> "TOKEN: read"
        TokenWrite -> "TOKEN: write"

    -- Literals
        TokenBoolLit value ->
            intercalate "\n"
                [ "TOKEN: Boolean Literal"
                , "VALUE: " ++ (if value then "true" else "false")
                ]
        TokenCharLit value ->
            intercalate "\n"
                [ "TOKEN: Character Literal"
                , "VALUE: " ++ showLitChar value ""
                ]
        TokenIntLit value ->
            intercalate "\n"
                [ "TOKEN: Integer Literal"
                , "VALUE: " ++ show value
                ]
        TokenFloatLit value ->
            intercalate "\n"
                [ "TOKEN: Float Literal"
                , "VALUE: " ++ show value
                ]
        TokenStringLit value ->
            intercalate "\n"
                [ "TOKEN: String Literal"
                , "VALUE: " ++ showLitString value ""
                ]

    -- Identifier
        TokenVarId name ->
            intercalate "\n"
                [ "TOKEN: Variable ID"
                , "VALUE: " ++ name
                ]
        TokenGenId name ->
            intercalate "\n"
                [ "TOKEN: General ID"
                , "VALUE: " ++ name
                ]

    -- Error
        ErrorUnderflow value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Underflow"
                , "VALUE: " ++ value
                ]
        ErrorOverflow value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Overflow"
                , "VALUE: " ++ value
                ]
        ErrorUnclosedStringLit value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Unclosed String Literal"
                , "VALUE: \"" ++ value
                ]
        ErrorUnexpectedToken value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Unexpected Token"
                , "VALUE: " ++ [value]
                ]

    -- EOF
        EOF -> "TOKEN: EOF"
