module Tokens (Position(..), Token(..))
where


data Position = Position Int Int
instance Show Position where
    show (Position l c) = "Line: " ++ show l ++ " Column: " ++ show c


data Token
    = 
    -- Logical Operators --
    TokenAnd Position    | TokenAndalso Position | TokenOr Position 
    | TokenOrelse Position | TokenNot Position

    -- Bitwise Operators
    | TokenBand Position | TokenBnot Position | TokenBor Position 
    | TokenBsl Position  | TokenBsr Position  | TokenBxor Position

    -- Array -- 
    | TokenLength Position  

    -- Arithmetic Operators --
    | TokenPlus Position     | TokenMinus Position           | TokenTimes Position 
    | TokenDivision Position | TokenIntegerDivision Position | TokenRem Position 

    -- Relational -- 
    | TokenLessThan Position    | TokenLessThanOrEqual Position 
    | TokenGreaterThan Position | TokenGreaterThanOrEqual Position

    -- Equality --

    | TokenEqualTo Position | TokenNotEqualTo Position

    -- Control Structures --
    | TokenEnd Position       | TokenFor Position | TokenIf Position 
    | TokenOtherwise Position | TokenWhile Position 

    -- Functions and Procedures --
    | TokenFinish Position | TokenFunction Position | TokenProcedure Position  | TokenReturn Position

    -- Composite Types -- 
    | TokenEither Position | TokenRecord Position 

    -- Conversion --
    | TokenToBoolean Position | TokenToCharacter Position | TokenToFloat Position | TokenToInteger Position 

    -- Types --
    | TokenBooleanType Position | TokenCharacterType Position |  TokenFloatType Position 
    | TokenIntegerType Position | TokenStringType Position    |  TokenVoidType Position

    -- Identifier --
    | TokenIdentifier String Position

    -- Punctuation -- 
    | TokenComma Position | TokenDot Position             | TokenSemiColon Position 
    | TokenArrow Position | TokenOpenParenthesis Position | TokenCloseParenthesis Position 

    -- Consts --
    | TokenCharacter Char Position | TokenFloat  Float  Position 
    | TokenInteger   Int  Position | TokenString String Position
    
    | TokenFalse Position | TokenTrue Position

    -- Assign --
    | TokenIs Position

    -- IO --
    | TokenPrint Position | TokenRead Position 

instance Show Token where
    show token = case token of
        TokenAnd     position     -> "Token AND\n"     ++ show position        
        TokenAndalso position -> "Token ANDALSO\n" ++ show position    
        TokenOr      position      -> "Token OR\n"      ++ show position         
        TokenOrelse  position  -> "Token ORELSE\n"  ++ show position     
        TokenNot     position     -> "Token NOT\n"     ++ show position       

    -- Bitwise Operators
    
        TokenBand position -> "Token BAND\n" ++ show position      
        TokenBnot position -> "Token BNOT\n" ++ show position      
        TokenBor  position  -> "Token BOR\n"  ++ show position       
        TokenBsl  position  -> "Token BSL\n"  ++ show position       
        TokenBsr  position  -> "Token BSR\n"  ++ show position       
        TokenBxor position -> "Token BXOR\n" ++ show position     

    -- Array -- 
    
        TokenLength position -> "Token LENGTH\n" ++ show position       

    -- Arithmetic Operators --
    
        TokenPlus            position -> "Token +\n"   ++ show position      
        TokenMinus           position -> "Token -\n"   ++ show position      
        TokenTimes           position -> "Token *\n"   ++ show position      
        TokenDivision        position -> "Token /\n"   ++ show position      
        TokenIntegerDivision position -> "Token DIV\n" ++ show position    
        TokenRem             position -> "Token REM\n" ++ show position    

    -- Relational -- 
    
        TokenLessThan           position -> "Token <\n"   ++ show position       
        TokenLessThanOrEqual    position -> "Token -><\n" ++ show position      
        TokenGreaterThan        position -> "Token >\n"   ++ show position       
        TokenGreaterThanOrEqual position -> "Token >->\n" ++ show position     

    -- Equality --

    
        TokenEqualTo    position -> "Token ->\n"  ++ show position       
        TokenNotEqualTo position -> "Token /->\n" ++ show position     

    -- Control Structures --
    
        TokenEnd       position -> "Token END\n"       ++ show position            
        TokenFor       position -> "Token FOR\n"       ++ show position            
        TokenIf        position -> "Token IF\n"        ++ show position            
        TokenOtherwise position -> "Token OTHERWISE\n" ++ show position    
        TokenWhile     position -> "Token WHILE\n"     ++ show position        

    -- Functions and Procedures --
    
        TokenFinish    position -> "Token FINISH\n"    ++ show position    
        TokenFunction  position -> "Token FUNCTION\n"  ++ show position    
        TokenProcedure position -> "Token PROCEDURE\n" ++ show position     
        TokenReturn    position -> "Token RETURN\n"    ++ show position   

    -- Composite Types -- 
    
        TokenEither position -> "Token EITHER\n" ++ show position      
        TokenRecord position -> "Token RECORD\n" ++ show position      

    -- Conversion --
    
        TokenToBoolean   position -> "Token TOBOOLEAN\n"   ++ show position    
        TokenToCharacter position -> "Token TOCHARACTER\n" ++ show position    
        TokenToFloat     position -> "Token TOFLOAT\n"     ++ show position    
        TokenToInteger   position -> "Token TOINTEGER\n"   ++ show position    

    -- Types --
    
        TokenBooleanType   position -> "Token BOOLEAN\n"   ++ show position    
        TokenCharacterType position -> "Token CHARACTER\n" ++ show position    
        TokenFloatType     position -> "Token FLOAT\n"     ++ show position    
    
        TokenIntegerType position -> "Token INTEGER\n" ++ show position    
        TokenStringType  position -> "Token STRING\n"  ++ show position         
        TokenVoidType    position -> "Token VOID\n"    ++ show position     

    -- Identifier --

        TokenIdentifier lexema position -> "Token IDENTIFIER\nLexema: " ++ lexema ++ "\n" ++ show position

    -- Punctuation -- 
    
        TokenComma            position -> "Token ,\n"  ++ show position     
        TokenDot              position -> "Token .\n"  ++ show position                 
        TokenSemiColon        position -> "Token ;\n"  ++ show position     
        TokenArrow            position -> "Token ->\n" ++ show position    
        TokenOpenParenthesis  position -> "Token (\n"  ++ show position     
        TokenCloseParenthesis position -> "Token )\n"  ++ show position     

    -- Consts --
    
        TokenCharacter value position -> "Token CHARACTER\nValue: " ++ show value ++ "\n" ++ show position    
        TokenFloat     value position -> "Token FLOAT\nValue: "     ++ show value ++ "\n" ++ show position    
        TokenInteger   value position -> "Token INTEGER\nValue: "   ++ show value ++ "\n" ++ show position    
        TokenString    value position -> "Token STRING\nValue: "    ++ show value ++ "\n" ++ show position   
    
    
        TokenFalse position -> "Token FALSE\n" ++ show position    
        TokenTrue  position -> "Token TRUE\n"  ++ show position    

    -- Assign --
    
        TokenIs position -> "Token IS\n" ++ show position     

    -- IO --
    
        TokenPrint position -> "Token PRINT\n" ++ show position    
        TokenRead  position  -> "Token READ\n"  ++ show position     