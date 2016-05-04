{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Expression
    ( Exp
    , Expression (..)
    , BinaryOp (..)
    , UnaryOp (..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Treelike

import           Data.Int                 (Int32)
--------------------------------------------------------------------------------

type Exp = At Expression

data Expression
    = LitBool   Bool
    | LitChar   Char
    | LitInt    Int32
    | LitFloat  Float
    | LitString String

    | Otherwise

    | VarId String

    | Binary (At BinaryOp) Exp Exp
    | Unary  (At UnaryOp)  Exp
    deriving (Eq, Show)

instance Treelike Expression where
    toTree = \case
        LitBool val ->
            Node (if val then "true" else "false") []
        LitChar val ->
            Node (show val) []
        LitInt val ->
            Node (show val) []
        LitFloat val ->
            Node (show val) []
        LitString val ->
            Node (show val) []

        Otherwise ->
            Node "otherwise" []

        VarId name ->
            Node ("Variable " ++ name) []

        Binary    (op :@ _) exp0 exp1 ->
            Node (show op) (toForest [exp0, exp1])
        Unary     (op :@ _) expr ->
            Node (show op) [toTree expr]

data BinaryOp
    = And | Andalso | Or | Orelse | Xor
    | Band | Bor | Bsl | Bsr | Bxor
    | Colon | Underscore
    | Plus | Minus | Times | FloatDiv | IntDiv | Rem
    | LTop | LEop | GTop | GEop | EQop | NEop | FAop | NFop
    deriving Eq

instance Show BinaryOp where
    show = \case
        And        -> "and"
        Andalso    -> "andalso"
        Or         -> "or"
        Orelse     -> "orelse"
        Xor        -> "xor"
        Band       -> "band"
        Bor        -> "bor"
        Bsl        -> "bsl"
        Bsr        -> "bsr"
        Bxor       -> "bxor"
        Colon      -> "(:)"
        Underscore -> "(_)"
        Plus       -> "(+)"
        Minus      -> "(-)"
        Times      -> "(*)"
        FloatDiv   -> "(/)"
        IntDiv     -> "div"
        Rem        -> "rem"
        LTop       -> "(<)"
        LEop       -> "(=<)"
        GTop       -> "(>)"
        GEop       -> "(>=)"
        EQop       -> "(=)"
        NEop       -> "(/=)"
        FAop       -> "(|)"
        NFop       -> "(!|)"

data UnaryOp
    = Not
    | Bnot
    | Length
    | Uminus
    | ToBoolean
    | ToCharacter
    | ToFloat
    | ToInteger
    deriving Eq

instance Show UnaryOp where
    show = \case
        Not         -> "not"
        Bnot        -> "bnot"
        Length      -> "length"
        Uminus      -> "(-)"
        ToBoolean   -> "toBoolean"
        ToCharacter -> "toCharacter"
        ToFloat     -> "toFloat"
        ToInteger   -> "toInteger"
