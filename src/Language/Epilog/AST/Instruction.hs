{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module Language.Epilog.AST.Instruction
    ( Instruction (..)
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
import           Language.Epilog.Common
import           Language.Epilog.Type
import           Language.Epilog.Position
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable                  (toList)
import           Data.Sequence                  (Seq)
--------------------------------------------------------------------------------

-- Sequence synonyms -----------------------------------------------------------
type Insts  = Seq Instruction

type Guard  = (Position, Expression, Insts)
type Guards = Seq Guard

type Set    = (Position, Exps, Insts)
type Sets   = Seq Set

type Range  = (Position, Expression, Expression, Insts)
type Ranges = Seq Range

-- Instructions ----------------------------------------------------------------
data Instruction
    = ProcDecl    Position Type         Name       Insts
    | Declaration Position Type         Name       (Maybe Expression)
    | Assign      Position Lval         Expression
    | ICall       Position Name         Exps

    | If          Position Guards
    | Case        Position Expression   Sets
    | For         Position (Maybe Type) Name       Ranges
    | While       Position Guards

    | Read        Position Lval
    | Write       Position Expression

    | Finish      Position
    deriving (Eq, Show)

instance P Instruction where
    pos = \case
        Declaration p _ _ _ -> p
        Assign      p _ _   -> p
        ICall       p _ _   -> p
        If          p _     -> p
        Case        p _ _   -> p
        For         p _ _ _ -> p
        While       p _     -> p
        Read        p _     -> p
        Write       p _     -> p
        Finish      p       -> p

instance Treelike Insts where 
    toTree insts = Node "Instructions" (toForest insts)

instance Treelike Instruction where
    toTree = \case
        ProcDecl p t name insts ->
                Node (unwords [name ++ " " ++ show t, showP p]) $
                (toForest insts)    
        Declaration p t var val ->
            Node (unwords ["Declaration", showP p]) $
                leaf (unwords ["Variable", var]) :
                leaf (show t) :
                (case val of
                    Nothing -> []
                    Just x  -> [Node "Initial value" [toTree x]])

        Assign p lval expr ->
            Node (unwords ["Assign", showP p])
                [toTree lval, toTree expr]

        ICall p proc args ->
            Node (unwords ["Instruction Call", proc, showP p])
                [Node "Arguments" (toList . fmap toTree $ args)]

        If p guards ->
            Node (unwords ["If", showP p])
                (toList . fmap guardTree $ guards)

        Case p var sets ->
            Node (unwords ["Case", showP p]) $
                toTree var :
                (toList . fmap setTree $ sets)

        For p Nothing var ranges ->
            Node (unwords ["For", showP p]) $
                leaf ("Variable " ++ var) :
                (toList . fmap rangeTree $ ranges)

        For p (Just t) var ranges ->
            Node (unwords ["For", showP p]) $
                Node "Declaration"
                    [ leaf (unwords ["Variable", var])
                    , toTree t
                    ] :
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
            leaf (unwords ["Finish", showP p])

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
