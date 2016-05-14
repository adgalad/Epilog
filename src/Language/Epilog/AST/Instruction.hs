{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Instruction
    ( Instruction (..)
    , Lval (..)
    , Type (..)
    , Cond
    , Conds
    , Exps
    , Guard
    , Guards
    , Inst
    , Insts
    , Range
    , Ranges
    , Set
    , Sets
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.At
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Foldable                  (toList)
import           Data.Sequence                  (Seq)
import           Data.Tree                      (flatten)
--------------------------------------------------------------------------------
-- Sequence synonyms -----------------------------------------------------------

type Inst   = At Instruction
type Insts  = Seq Inst

type Guard  = (Exp, Insts)
type Guards = Seq Guard

type Exps   = Seq Exp
type Set    = (Exps, Insts)
type Sets   = Seq Set

type Range  = (Exp, Exp, Insts)
type Ranges = Seq Range

type Cond   = (Exp, Insts)
type Conds  = Seq Cond

-- Instructions ----------------------------------------------------------------

data Instruction
    = Declaration (At Type) (At String) (Maybe Exp)
    | Assign Lval Exp
    | Call (At String) Exps

    | If Guards
    | Case Exp Sets
    | For  Exp Ranges
    | ForD Inst Ranges
    | While Conds

    | Read (At Lval)
    | Write Exp

    | Finish
    deriving (Eq, Show)

instance Treelike Instruction where
    toTree = \case
        Declaration (t_pe :@ _ ) (var :@ _ ) val ->
            Node "Declaration" $
                Node ("Variable " ++ var) [] :
                toTree t_pe :
                (case val of
                    Nothing ->
                        []
                    Just x ->
                        [Node "Initial value" [toTree x]])

        Assign lval expr ->
            Node "Assign" [toTree lval, toTree expr]

        Call (proc :@ _) args ->
            Node
                ("Call " ++ proc)
                [Node "Arguments" (toList . fmap toTree $ args)]

        If guards ->
            Node "If" (toList . fmap ifTree $ guards)

        Case (var :@ _) sets ->
            Node "Case" $ toTree var:(toList . fmap caseTree $ sets)

        For (var :@ _) ranges ->
            Node "For" $ toTree var:(toList . fmap forTree $ ranges)

        ForD decl ranges ->
            Node "For" $ toTree decl:(toList . fmap forTree $ ranges)

        While conds ->
            Node "While" (toList . fmap whileTree $ conds )

        Read lval ->
            Node "Read" [toTree lval]

        Write expr ->
            Node "Write" [toTree expr]

        Finish ->
            Node "Finish" []

        where
            ifTree :: Guard -> Tree String
            ifTree (cond, insts) =
                Node "Guard"
                    [ Node "Condition" [toTree cond]
                    , Node "Body" (toForest insts)
                    ]

            caseTree :: Set -> Tree String
            caseTree (expr, insts) =
                Node "Set"
                    [ Node "Values" (toForest expr)
                    , Node "Body" (toForest insts)
                    ]

            forTree :: Range -> Tree String
            forTree (lower, upper, insts) =
                Node "Range"
                    [ Node "From" [toTree lower]
                    , Node "To"   [toTree upper]
                    , Node "Body" (toForest insts)
                    ]

            whileTree :: Cond -> Tree String
            whileTree (expr, insts) =
                Node "Branch"
                    [ Node "Condition" [toTree expr]
                    , Node "Body" (toForest insts)
                    ]

-- Lval ------------------------------------------------------------------------

data Lval
    = Variable (At String)
    | Member Lval (At String)
    | Entry Lval Exp
    deriving (Eq, Show)

instance Treelike Lval where
    toTree = aux1 . reverse . aux0
        where
            aux0 = \case
                Variable (name :@ _)  ->
                    [name]
                Member lval (member :@ _) ->
                    ('_': member) : aux0 lval
                Entry lval  (entry :@ _)  ->
                    (':': (show . flatten . toTree $ entry)) : aux0 lval

            aux1 (x:y:xs) =
                Node x [aux1 (y:xs)]
            aux1 [x] =
                Node x []
            aux1 [] =
                Node "" []
