{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.AST.Expression
    ( Expression(..)
     ,BinaryOperator(..)
     ,UnaryOperator(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At

import           Data.Int               (Int32)
--------------------------------------------------------------------------------

data Expression
    = LitChar   (At Char  )
    | LitBool   (At Bool  )
    | LitInt    (At Int32 )
    | LitFloat  (At Float )
    | LitString (At String)
    | Otherwise (At ()    )
    | VarId     (At String)
    | GenId     (At String)
    | BinaryExp (At BinaryOperator) (At Expression) (At Expression)
    | UnaryExp  (At UnaryOperator)  (At Expression)
    deriving (Eq)

instance Show Expression where
    show = \case
        LitBool   value    -> "LitBool "   ++ show (item value)
        LitChar   value    -> "LitChar "   ++ [item value]
        LitInt    value    -> "LitInt "    ++ show (item value)
        LitFloat  value    -> "LitFloat "  ++ show (item value)
        LitString value    -> "LitString " ++ item value
        Otherwise _        -> "Otherwise"
        VarId     value    -> "VarId "     ++ item value
        GenId     value    -> "GenId "     ++ item value
        BinaryExp op e1 e2 -> "(" ++ show (item e1) ++ " " ++ show (item op) ++ " " ++ show  (item e2) ++ ")"
        UnaryExp  op e     -> "(" ++ show (item op) ++ " " ++ show (item e) ++ ")"

data BinaryOperator
    = And | Andalso | Or | Orelse | Xor
    | Band | Bor | Bsl | Bsr | Bxor
    | Colon | Underscore
    | Plus | Minus | Times | FloatDiv | IntDiv | Rem
    | LTop | LEop | GTop | GEop | EQop | NEop | Factor | NotFactor
    deriving (Eq, Show)

data UnaryOperator
    = Not
    | Bnot
    | Length
    | Uminus
    deriving (Eq, Show)
