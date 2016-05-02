{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.AST.Expression
    ( Exp
    , Expression(..)
    , BinaryOp(..)
    , UnaryOp(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.At
import           Language.Epilog.Treelike

import           Data.Int               (Int32)
--------------------------------------------------------------------------------

type Exp = At Expression

data Expression
    = LitBool   (At Bool  )
    | Otherwise (At ()    )
    | LitChar   (At Char  )
    | LitInt    (At Int32 )
    | LitFloat  (At Float )
    | LitString (At String)

    | VarId (At String)
    | GenId (At String)

    | ToBoolean   Exp
    | ToCharacter Exp
    | ToFloat     Exp
    | ToInteger   Exp

    | Binary (At BinaryOp) Exp Exp
    | Unary  (At UnaryOp)  Exp
    deriving (Eq)

instance Show Expression where
    show = \case
        LitBool   value    -> "LitBool "   ++ show (item value)
        Otherwise _        -> "Otherwise"
        LitChar   value    -> "LitChar "   ++ [item value]
        LitInt    value    -> "LitInt "    ++ show (item value)
        LitFloat  value    -> "LitFloat "  ++ show (item value)
        LitString value    -> "LitString " ++ item value

        ToBoolean   value  -> "ToBoolean " ++ show (item value)
        ToCharacter value  -> "ToCharacter " ++ show (item value)
        ToFloat     value  -> "ToFloat " ++ show (item value)
        ToInteger   value  -> "ToInteger " ++ show (item value)

        VarId     value    -> "VarId "     ++ item value
        GenId     value    -> "GenId "     ++ item value

        Binary    op e1 e2 -> "(" ++ show (item e1) ++ " " ++ show (item op) ++ " " ++ show  (item e2) ++ ")"
        Unary     op e     -> "(" ++ show (item op) ++ " " ++ show (item e) ++ ")"

instance Treelike Expression where
    toTree = \case
        LitBool (val   :@ _) ->
            Node (if val then "true" else "false") []
        Otherwise (()  :@ _) ->
            Node "otherwise" []
        LitChar (val :@ _) ->
            Node (show val) []
        LitInt (val :@ _) ->
            Node (show val) []
        LitFloat (val :@ _) ->
            Node (show val) []
        LitString (val :@ _) ->
            Node (show val) []

        VarId (name :@ _) ->
            Node (show name) []
        GenId (name :@ _) ->
            Node (show name) []

        ToBoolean   expr ->
            Node "toBoolean" [ toTree expr]
        ToCharacter expr ->
            Node "toCharacter" [ toTree expr]
        ToFloat     expr ->
            Node "toFloat" [ toTree expr]
        ToInteger   expr ->
            Node "toInteger" [ toTree expr]

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
        Colon      -> ":"
        Underscore -> "_"
        Plus       -> "+"
        Minus      -> "-"
        Times      -> "*"
        FloatDiv   -> "/"
        IntDiv     -> "div"
        Rem        -> "rem"
        LTop       -> "<"
        LEop       -> "=<"
        GTop       -> ">"
        GEop       -> ">="
        EQop       -> "="
        NEop       -> "/="
        FAop       -> "|"
        NFop       -> "!|"

data UnaryOp
    = Not
    | Bnot
    | Length
    | Uminus
    deriving Eq

instance Show UnaryOp where
    show = \case
        Not    -> "not"
        Bnot   -> "bnot"
        Length -> "length"
        Uminus -> "(-)"
