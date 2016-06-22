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

    -- | Otherwise Position

    | Lval      Position Lval

    | ECall     Position Name     Exps

    | Binary    Position BinaryOp Expression Expression
    | Unary     Position UnaryOp  Expression
    deriving (Eq, Show)

instance P Expression where
    pos = \case
        LitBool   p _     -> p -- AST built
        LitChar   p _     -> p -- AST built
        LitInt    p _     -> p -- AST built
        LitFloat  p _     -> p -- AST built
        LitString p _     -> p -- AST built
     -- Otherwise p       -> p
        Lval      p _     -> p -- AST built
        ECall     p _ _   -> p -- AST built
        Binary    p _ _ _ -> p -- AST built
        Unary     p _ _   -> p -- AST built

instance Treelike Expression where
    toTree = \case
        LitBool _ val ->
            leaf (unwords [if val then "true" else "false"])

        LitChar _ val ->
            leaf (unwords [show val])

        LitInt _ val ->
            leaf (unwords [show val])

        LitFloat _ val ->
            leaf (unwords [show val])

        LitString _ val ->
            leaf (unwords [show val])

        -- Otherwise _ ->
        --     leaf (unwords ["otherwise"])

        Lval _ lval ->
            Node (unwords ["Lval"]) [toTree lval]

        ECall _ proc args ->
            Node (unwords ["Expression Call", proc])
                [Node "Arguments" (toList . fmap toTree $ args)]

        Binary _ op exp0 exp1 ->
            Node (unwords [show op]) (toForest [exp0, exp1])

        Unary _ op expr ->
            Node (unwords [show op]) [toTree expr]

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
    = Variable Name            -- AST built
    | Member   Lval Name       -- AST built
    | Index    Lval Expression -- AST built
    | Deref    Lval            -- AST built
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
                    (show . flatten . toTree $ index) : aux0 lval
                Deref lval ->
                    "^" : aux0 lval

            aux1 (x:y:xs) =
                Node x [aux1 (y:xs)]
            aux1 [x] =
                leaf x
            aux1 [] =
                leaf ""
