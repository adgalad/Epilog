{-# LANGUAGE LambdaCase #-}

module Language.Epilog.Token
  ( Token(..)
  ) where
--------------------------------------------------------------------------------
data Token

  -- Logical Operators
  = TokenAnd | TokenAndalso | TokenOr | TokenOrelse | TokenNot

  -- Bitwise Operators
  | TokenBand | TokenBnot | TokenBor | TokenBsl  | TokenBsr  | TokenBxor

  -- Array
  | TokenLength | TokenColon

  -- Arithmetic Operators
  | TokenPlus | TokenMinus | TokenTimes | TokenDivision | TokenRem
  | TokenIntegerDivision

  -- Relational
  | TokenLessThan | TokenLessThanOrEqual | TokenGreaterThan
  | TokenGreaterThanOrEqual | TokenFactorOf

  -- Equality
  | TokenEqualTo | TokenNotEqualTo

  -- Control Structures
  | TokenEnd | TokenFor   | TokenIf | TokenOtherwise | TokenWhile

  -- Functions and Procedures
  | TokenFinish | TokenFunction | TokenProcedure | TokenReturn
  | TokenDefine

  -- Composite Types
  | TokenEither | TokenRecord

  -- Conversion
  | TokenToBoolean | TokenToCharacter | TokenToFloat   | TokenToInteger

  -- Types
  | TokenBooleanType | TokenCharacterType | TokenFloatType
  | TokenIntegerType | TokenStringType    | TokenVoidType

  -- Identifier
  | TokenVariableIdentifier { unTokenVariableIdentifier :: String }
  | TokenGeneralIdentifier { unTokenGeneralIdentifier :: String }

  -- Punctuation
  | TokenComma      | TokenDot             | TokenSemiColon
  | TokenArrow      | TokenOpenParenthesis | TokenCloseParenthesis
  | TokenUnderscore | TokenOpenCurly | TokenCloseCurly

  -- Consts
  | TokenCharacter { unTokenCharacter :: Char   }
  | TokenFloat     { unTokenFloat     :: Float  }
  | TokenInteger   { unTokenInteger   :: Int    }
  | TokenString    { unTokenString    :: String }
  | TokenBoolean   { unTokenBool      :: Bool   }

  -- Assign
  | TokenIs

  -- IO
  | TokenPrint | TokenRead

  -- Error
  | TokenError String

  -- EOF
  | TokenEOF
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
    TokenDivision        -> "Token /"
    TokenIntegerDivision -> "Token DIV"
    TokenRem             -> "Token REM"

  -- Relational
    TokenLessThan           -> "Token <"
    TokenLessThanOrEqual    -> "Token =<"
    TokenGreaterThan        -> "Token >"
    TokenGreaterThanOrEqual -> "Token >="
    TokenFactorOf           -> "Token |"

  -- Equality
    TokenEqualTo    -> "Token ="
    TokenNotEqualTo -> "Token /="

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
      "Token VARIABLEIDENTIFIER " ++ name
    TokenGeneralIdentifier name ->
      "Token GENERALIDENTIFIER " ++ name

  -- Punctuation
    TokenComma            -> "Token ,"
    TokenDot              -> "Token ."
    TokenSemiColon        -> "Token ;"
    TokenArrow            -> "Token ->"
    TokenOpenParenthesis  -> "Token ("
    TokenCloseParenthesis -> "Token )"
    TokenOpenCurly        -> "Token {"
    TokenCloseCurly       -> "Token }"
    TokenUnderscore       -> "Token _"

  -- Consts
    TokenCharacter value ->
      "Token CHARACTER (" ++ show value ++ ")"
    TokenFloat value ->
      "Token FLOAT (" ++ show value ++ ")"
    TokenInteger value ->
      "Token INTEGER (" ++ show value ++ ")"
    TokenString value ->
      "Token STRING (" ++ show value ++ ")"
    TokenBoolean value ->
      "Token BOOLEAN (" ++ show value ++ ")"

  -- Assign
    TokenIs -> "Token IS"

  -- IO
    TokenPrint -> "Token PRINT"
    TokenRead  -> "Token READ"

  -- Error
    TokenError msg ->
      "Error: " ++ msg

  -- EOF
    TokenEOF -> "Token EOF"
