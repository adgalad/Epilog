{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.Expression
    ( Expression(..)
     ,BinaryOperator(..)
     ,UnaryOperator(..)
    ) where
         
--------------------------------------------------------------------------------
import           Language.Epilog.Lexeme

import           Data.Int               (Int32)
--------------------------------------------------------------------------------

data Expression
    = LitChar   (Lexeme Char)
    | LitBool   (Lexeme Bool)
    | LitInt    (Lexeme Int32)
    | LitFloat  (Lexeme Float)
    | LitString (Lexeme String)
    | BinaryExp (Lexeme BinaryOperator) (Lexeme Expression) (Lexeme Expression)
    | UnaryExp  (Lexeme UnaryOperator)  (Lexeme Expression)
    deriving (Eq)

instance Show Expression where
    show = \case
        LitBool   value     -> "LitBool "   ++ show (token value)
        LitChar   value     -> "LitChar "   ++ show (token value)
        LitInt    value     -> "LitInt "    ++ show (token value)
        LitFloat  value     -> "LitFloat "  ++ show (token value)
        LitString value     -> "LitString " ++ show (token value)
        BinaryExp op e1 e2  -> "(" ++ show (token e1) ++ " " ++ show (token op) ++ " " ++ show  (token e2) ++ ")"
        UnaryExp  op e      -> "(" ++ show (token op) ++ " " ++ show (token e) ++ ")"


data BinaryOperator 
    = Plus | Minus | Times | FloatDivision| IntegerDivision | Rem
    | And  | Or | Andalso | Orelse | Band | Bor | Bsl | Bsr | Bxor        
    deriving (Eq, Show)

data UnaryOperator 
    = Uminus | Not | Bnot | Length
    deriving (Eq, Show)

