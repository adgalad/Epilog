{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Expression
    ( Expression (..)
    , Lval (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , Exps
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Position
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Sequence            (Seq)
import           Data.Tree                (flatten)
--------------------------------------------------------------------------------
-- Useful synonyms ---------------------
type Exps   = Seq Expression
type Name = String

-- Expressions -------------------------
data Expression
    = LitBool   Position Bool
    | LitChar   Position Char
    | LitInt    Position Int32
    | LitFloat  Position Float
    | LitString Position String

    | Otherwise Position

    | Lval      Position Lval

    | ECall     Position Name     Exps

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
        ECall     p _ _   -> p
        Binary    p _ _ _ -> p
        Unary     p _ _   -> p

instance Treelike Expression where
    toTree = \case
        LitBool p val ->
            leaf (unwords [(if val then "true" else "false"), showP p])

        LitChar p val ->
            leaf (unwords [show val, showP p])

        LitInt p val ->
            leaf (unwords [show val, showP p])

        LitFloat p val ->
            leaf (unwords [show val, showP p])

        LitString p val ->
            leaf (unwords [show val, showP p])

        Otherwise p ->
            leaf (unwords ["otherwise", showP p])

        Lval p lval ->
            Node (unwords ["Lval", showP p]) [toTree lval]

        ECall p proc args ->
            Node (unwords ["Expression Call", proc, showP p])
                [Node "Arguments" (toList . fmap toTree $ args)]

        Binary p op exp0 exp1 ->
            Node (unwords [show op, showP p]) (toForest [exp0, exp1])

        Unary p op expr ->
            Node (unwords [show op, showP p]) [toTree expr]

data BinaryOp
    = And | Andalso | Or | Orelse | Xor
    | Band | Bor | Bsl | Bsr | Bxor
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
    | Uminus
    deriving Eq

instance Show UnaryOp where
    show = \case
        Not         -> "not"
        Bnot        -> "bnot"
        Uminus      -> "(-)"

-- Lval ------------------------------------------------------------------------

data Lval
    = Variable Name
    | Member   Lval Name
    | Index    Lval Expression
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
                leaf x
            aux1 [] =
                leaf ""
