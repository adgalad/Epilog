{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Instruction
    ( Instruction (..)
    , Lval (..)
    , Type (..)
    , Exps
    , Guard
    , Guards
    , Insts
    , Range
    , Ranges
    , Set
    , Sets
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.Position
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable                  (toList)
import           Data.Sequence                  (Seq)
import           Data.Tree                      (flatten)
--------------------------------------------------------------------------------
-- Sequence synonyms -----------------------------------------------------------

type Insts  = Seq Instruction

type Guard  = (Position, Expression, Insts)
type Guards = Seq Guard

type Exps   = Seq Expression
type Set    = (Position, Exps, Insts)
type Sets   = Seq Set

type Range  = (Position, Expression, Expression, Insts)
type Ranges = Seq Range

-- Other synonyms --------------------------------------------------------------
type Name = String

-- Instructions ----------------------------------------------------------------
data Instruction
    = Declaration Position Type        Name       (Maybe Expression)
    | Assign      Position Lval        Expression
    | Call        Position Name        Exps

    | If          Position Guards
    | Case        Position Expression  Sets
    | For         Position Name        Ranges
    | ForD        Position Instruction Ranges
    | While       Position Guards

    | Read        Position Lval
    | Write       Position Expression

    | Finish      Position
    deriving (Eq, Show)

instance P Instruction where
    pos = \case
        Declaration p _ _ _ -> p
        Assign      p _ _   -> p
        Call        p _ _   -> p
        If          p _     -> p
        Case        p _ _   -> p
        For         p _ _   -> p
        ForD        p _ _   -> p
        While       p _     -> p
        Read        p _     -> p
        Write       p _     -> p
        Finish      p       -> p

instance Treelike Instruction where
    toTree = \case
        Declaration p t var val ->
            Node (unwords ["Declaration", showP p]) $
                Node (unwords ["Variable", var]) [] :
                toTree t :
                (case val of
                    Nothing -> []
                    Just x  -> [Node "Initial value" [toTree x]])

        Assign p lval expr ->
            Node (unwords ["Assign", showP p])
                [toTree lval, toTree expr]

        Call p proc args ->
            Node (unwords ["Call", proc, showP p])
                [Node "Arguments" (toList . fmap toTree $ args)]

        If p guards ->
            Node (unwords ["If", showP p])
                (toList . fmap guardTree $ guards)

        Case p var sets ->
            Node (unwords ["Case", showP p]) $
                toTree var :
                (toList . fmap setTree $ sets)

        For p var ranges ->
            Node (unwords ["For", showP p]) $
                Node ("Variable " ++ var) [] :
                (toList . fmap rangeTree $ ranges)

        ForD p decl ranges ->
            Node (unwords ["For", showP p]) $
                toTree decl :
                (toList . fmap rangeTree $ ranges)

        While p guards ->
            Node (unwords ["While", showP p])
                (toList . fmap guardTree $ guards )

        Read p lval ->
            Node (unwords ["Read", showP p])
                [toTree lval]

        Write p expr ->
            Node (unwords ["Write", showP p])
                [toTree expr]

        Finish p ->
            Node (unwords ["Finish", showP p]) []

        where
            guardTree :: Guard -> Tree String
            guardTree (p, cond, insts) =
                Node (unwords ["Guard", showP p])
                    [ Node "Condition" [toTree cond]
                    , Node "Body" (toForest insts)
                    ]

            setTree :: Set -> Tree String
            setTree (p, exprs, insts) =
                Node (unwords ["Set", showP p])
                    [ Node "Values" (toForest exprs)
                    , Node "Body" (toForest insts)
                    ]

            rangeTree :: Range -> Tree String
            rangeTree (p, lower, upper, insts) =
                Node (unwords ["Range", showP p])
                    [ Node "From" [toTree lower]
                    , Node "To"   [toTree upper]
                    , Node "Body" (toForest insts)
                    ]

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
