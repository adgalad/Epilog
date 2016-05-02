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
    | Declaration (At Type) Exp
    | Initialization (At Type) (At Instruction)

    | If Guards
    | Case Exp Sets
    | For  Exp Ranges
    | ForD (At Instruction) Ranges
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
            Node "Assign" [ toTree exp0, toTree exp1 ]
        Declaration (t_pe :@ typePos) var -> declTree t_pe var
        Initialization (t_pe :@ typePos) (inst :@ instPos) ->
            Node (show t_pe) [ toTree inst ]

        If guards ->
            Node "If" (toList . fmap ifTree $ guards)
        Case (var :@ varPos) sets ->
            Node "Case" $ (toTree var):(toList . fmap caseTree $ sets)
        For var ranges ->
            Node "For" $ (toTree var):(toList . fmap forTree $ ranges) 
        ForD decl ranges ->
            Node "For" $ (toTree decl):(toList . fmap forTree $ ranges) 
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
            declTree :: Type -> Exp -> Tree String 
            declTree t var = case t of   
                Array (t_pe :@ typePos) s  ->
                    Node "Array" 
                        [ Node (show t_pe) []
                        , Node "Size" (toForest s)
                        , toTree var 
                        ]
                otherwise -> Node (show t) [ toTree var ]
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


data Type
    = IntT | CharT | FloatT | BoolT | StringT | Array (At Type) Exps   deriving (Eq, Show)

instance Treelike Type where
    toTree = \case 
        Array t sizes -> 
            Node "Array" 
                [ Node (show t) []
                , Node "Size" (toForest sizes)
                ]

