{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Expression
    ( Expression (..)
    , Lval (..)
    , BinaryOp (..)
    , UnaryOp (..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Treelike
import           Language.Epilog.Position
--------------------------------------------------------------------------------
import           Data.Int                 (Int32)
import           Data.Tree                (flatten)
--------------------------------------------------------------------------------

data Expression
    = LitBool   Position Bool
    | LitChar   Position Char
    | LitInt    Position Int32
    | LitFloat  Position Float
    | LitString Position String

    | Otherwise Position

    | Lval      Position Lval

    | Binary    Position BinaryOp Expression Expression
    | Unary     Position UnaryOp  Expression
    deriving (Eq, Show)

instance P Expression where
    pos = \case
        LitBool   p _     -> p
        LitChar   p _     -> p
        LitInt    p _     -> p
        LitFloat  p _     -> p
        LitString p _     -> p
        Otherwise p       -> p
        Lval      p _     -> p
        Binary    p _ _ _ -> p
        Unary     p _ _   -> p

instance Treelike Expression where
    toTree = \case
        LitBool p val ->
            Node (unwords [(if val then "true" else "false"), showP p]) []
        LitChar p val ->
            Node (unwords [show val, showP p]) []
        LitInt p val ->
            Node (unwords [show val, showP p]) []
        LitFloat p val ->
            Node (unwords [show val, showP p]) []
        LitString p val ->
            Node (unwords [show val, showP p]) []

        Otherwise p ->
            Node (unwords ["otherwise", showP p]) []

        Lval p lval ->
            Node (unwords ["Lval", showP p]) [toTree lval]

        Binary p op exp0 exp1 ->
            Node (unwords [show op, showP p]) (toForest [exp0, exp1])
        Unary p op expr ->
            Node (unwords [show op, showP p]) [toTree expr]

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

-- Lval ------------------------------------------------------------------------

data Lval
    = Variable String
    | Member Lval String
    | Index Lval Expression
    deriving (Eq, Show)

instance Treelike Lval where
    toTree = aux1 . reverse . aux0
        where
            aux0 = \case
                Variable name ->
                    [name]
                Member lval member ->
                    ('_': member) : aux0 lval
                Index lval index ->
                    (':': (show . flatten . toTree $ index)) : aux0 lval

            aux1 (x:y:xs) =
                Node x [aux1 (y:xs)]
            aux1 [x] =
                Node x []
            aux1 [] =
                Node "" []
