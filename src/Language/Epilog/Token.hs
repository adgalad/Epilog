module Language.Epilog.Token
    ( Token(..)
    , isError
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Common
--------------------------------------------------------------------------------
import           Data.Char              (showLitChar)
import           Data.Int               (Int32)
import           Data.List              (intercalate)
import           GHC.Show               (showLitString)
--------------------------------------------------------------------------------

data Token
    -- Operators
    ---- Logical
    = TokenAnd | TokenAndalso | TokenOr | TokenOrelse | TokenXor | TokenNot

    ---- Bitwise
    | TokenBand | TokenBor | TokenBnot | TokenBsl  | TokenBsr  | TokenBxor

    ---- Array / Record / Either / Pointer
    | TokenUnderscore
    | TokenLeftBracket | TokenRightBracket
    | TokenLeftBrace   | TokenRightBrace
    | TokenCaret

    ---- Arithmetic
    | TokenPlus | TokenMinus | TokenTimes | TokenFloatDiv
    | TokenIntDiv | TokenRem

    ---- Relational
    | TokenLT | TokenLE | TokenGT | TokenGE
    | TokenEQ | TokenNE
    | TokenFA | TokenNF

    -- Control Structures
    | TokenEnd | TokenFor | TokenFrom | TokenTo | TokenIf | TokenOtherwise
    | TokenWhile -- | TokenCase | TokenOf

    -- Procedures
    | TokenProcedure | TokenDefine | TokenFinish | TokenAnswer

    -- Composite Types
    | TokenEither | TokenRecord

    -- Global Declaration
    | TokenGlobal

    -- Punctuation
    | TokenComma | TokenPeriod  | TokenSemicolon
    | TokenArrow | TokenLeftPar | TokenRightPar

    -- Assignment
    | TokenIs

    -- IO
    |  TokenRead | TokenWrite

    -- Literals
    | TokenBoolLit   { unTokenBoolLit :: Bool   }
    | TokenCharLit   { unTokenCharLit :: Char   }
    | TokenIntLit    { unTokenIntLit :: Int32  }
    | TokenFloatLit  { unTokenFloatLit :: Float  }
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
isError (ErrorUnderflow         _) = True
isError (ErrorOverflow          _) = True
isError (ErrorUnclosedStringLit _) = True
isError (ErrorUnexpectedToken   _) = True
isError _                          = False

instance Show Token where
    show t = "TOKEN: " <> case t of

    -- Operators
    ---- Logical
        TokenAnd     -> "and"
        TokenAndalso -> "andalso"
        TokenOr      -> "or"
        TokenOrelse  -> "orelse"
        TokenXor     -> "xor"
        TokenNot     -> "not"

    ---- Bitwise
        TokenBand -> "band"
        TokenBor  -> "bor"
        TokenBnot -> "bnot"
        TokenBsl  -> "bsl"
        TokenBsr  -> "bsr"
        TokenBxor -> "bxor"

    ---- Array / Record / Either / Pointer
        TokenUnderscore   -> "_"
        TokenLeftBracket  -> "["
        TokenRightBracket -> "]"
        TokenLeftBrace    -> "{"
        TokenRightBrace   -> "}"
        TokenCaret        -> "^"

    ---- Arithmetic
        TokenPlus     -> "+"
        TokenMinus    -> "-"
        TokenTimes    -> "*"
        TokenFloatDiv -> "/"
        TokenIntDiv   -> "div"
        TokenRem      -> "rem"

    ---- Relational
        TokenLT -> "<"
        TokenLE -> "=<"
        TokenGT -> ">"
        TokenGE -> ">="
        TokenEQ -> "="
        TokenNE -> "/="
        TokenFA -> "|"
        TokenNF -> "!|"

    -- Control Structures
        TokenEnd       -> "end"
        TokenFor       -> "for"
        TokenFrom      -> "from"
        TokenTo        -> "to"
        TokenIf        -> "if"
        TokenOtherwise -> "otherwise"
        TokenWhile     -> "while"
        -- TokenCase      -> "case"
        -- TokenOf        -> "of"

    -- Procedures
        TokenProcedure -> "procedure"
        TokenDefine    -> ":-"
        TokenFinish    -> "finish"
        TokenAnswer    -> "answer"

    -- Composite Types
        TokenEither -> "either"
        TokenRecord -> "record"

    -- Global Declaration
        TokenGlobal -> "global"

    -- Punctuation
        TokenComma     -> ","
        TokenPeriod    -> "."
        TokenSemicolon -> ";"
        TokenArrow     -> "->"
        TokenLeftPar   -> "("
        TokenRightPar  -> ")"

    -- Assign
        TokenIs -> "is"

    -- IO
        TokenRead  -> "read"
        TokenWrite -> "write"

    -- Literals
        TokenBoolLit value ->
            intercalate "\n"
                [ "Boolean Literal"
                , "VALUE: " <> (if value then "true" else "false")
                ]
        TokenCharLit value ->
            intercalate "\n"
                [ "Character Literal"
                , "VALUE: " <> showLitChar value ""
                ]
        TokenIntLit value ->
            intercalate "\n"
                [ "Integer Literal"
                , "VALUE: " <> show value
                ]
        TokenFloatLit value ->
            intercalate "\n"
                [ "Float Literal"
                , "VALUE: " <> show value
                ]
        TokenStringLit value ->
            intercalate "\n"
                [ "String Literal"
                , "VALUE: " <> showLitString value ""
                ]

    -- Identifier
        TokenVarId name ->
            intercalate "\n"
                [ "Variable ID"
                , "VALUE: " <> name
                ]
        TokenGenId name ->
            intercalate "\n"
                [ "General ID"
                , "VALUE: " <> name
                ]

    -- Error
        ErrorUnderflow value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Underflow"
                , "VALUE: " <> value
                ]
        ErrorOverflow value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Overflow"
                , "VALUE: " <> value
                ]
        ErrorUnclosedStringLit value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Unclosed String Literal"
                , "VALUE: \"" <> value
                ]
        ErrorUnexpectedToken value ->
            intercalate "\n"
                [ "ERROR"
                , "REASON: Unexpected Token"
                , "VALUE: " <> [value]
                ]

    -- EOF
        EOF -> "EOF"
