{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Instruction
    ( Inst
    , Insts
    , Guard
    , Guards
    , Exps
    , Set
    , Sets
    , Range
    , Ranges
    , Cond
    , Conds
    , Instruction(..)
    , Type(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.At
import           Language.Epilog.Treelike

import           Data.Foldable (toList)
import           Data.Sequence                  (Seq)
--------------------------------------------------------------------------------

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

data Instruction
    = EmptyInst
    | Assign Exp Exp
    | Declaration (At Type) (At String)
    | Initialization (At Type) (At Instruction)

    | If Guards
    | Case (At String) Sets
    | For (At String) Ranges
    | While Conds

    | Read Exp
    | Write Exp

    | Finish
    | Return Exp
    deriving (Eq, Show)

-- instance Show Instruction where
--     show = \case
--         Assign varid value -> "Assign " ++ show (item varid) ++ " " ++ show (item value)
--         Declaration t varid -> show (item t) ++ " " ++ item varid
--         Initialization t assign -> show (item t) ++ " (" ++ show (item assign) ++ ")"

--         If glist -> "If \n"++ show glist ++ "\nEnd"
--         Case var sets -> "Case \n"++ show (item var) ++ show sets  ++ "\nEnd"
--         For var ranges -> "For \n"++ show (item var) ++ show ranges ++ "\nEnd"
--         While conds -> "While \n"++ show conds ++ "\nEnd"

--         Read expr -> "Read \n" ++ show (item expr)
--         Write expr -> "Write \n" ++ show (item expr)

--         Finish -> "Finish"
--         Return value -> "Return " ++ show (item value)
--     -- Declaration type id

instance Treelike Instruction where
    toTree = \case
        Assign exp0 exp1 ->
            Node "UNDEFINED" []

        Declaration (t_pe :@ typePos) (string :@ stringPos) ->
            Node "UNDEFINED" []
        Initialization (t_pe :@ typePos) (inst :@ instPos) ->
            Node "UNDEFINED" []

        If guards ->
            Node "UNDEFINED" []
        Case (string :@ stringPos) sets ->
            Node "UNDEFINED" []
        For (string :@ stringPos) ranges ->
            Node "UNDEFINED" []
        While conds ->
            Node "While" (toList . fmap whileTree $ conds )

        Read expr ->
            Node "Read" [toTree expr]
        Write expr ->
            Node "Write" [toTree expr]

        Finish ->
            Node "Finish" []
        Return expr ->
            Node "Return" [toTree expr]

        where
            whileTree :: Cond -> Tree String
            whileTree (expr, insts) =
                Node "Branch"
                    [ Node "Condition" [toTree expr]
                    , Node "Body" (toForest insts)
                    ]

data Type
    = IntT | CharT | FloatT | BoolT | StringT deriving (Eq, Show)
