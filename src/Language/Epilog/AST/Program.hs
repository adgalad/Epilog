{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Program
    ( Program (..)
    , Declaration (..)
    , Dec
    , Decs
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Expression
import           Language.Epilog.At
import           Language.Epilog.Treelike

import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq (empty)
--------------------------------------------------------------------------------

type Dec = At Declaration
type Decs = Seq Dec

data Declaration
    = GlobalD Inst
    | EitherD (At String) Insts
    | RecordD (At String) Insts
    | ProcD
        { procName  :: At String
        , procVars  :: Insts
        , procInsts :: Insts
        }
    | FunD
        { funName  :: At String
        , funVars  :: Insts
        , funType  :: At Type
        , funInsts :: Insts
        }
    deriving (Eq, Show)

instance Treelike Declaration where
    toTree = \case
        GlobalD var -> Node "Global" [toTree var]
        EitherD (name :@_) insts -> 
            Node ("Either "++name) [ Node "Body" (toForest insts) ]
        RecordD (name :@_) insts -> 
            Node ("Record "++name) [ Node "Body" (toForest insts) ]
        ProcD (name :@_) param insts ->
            Node (unwords ["Procedure", name])
                [ paramTree param
                , Node "Body" (toForest insts)
                ]
        FunD  (name :@_) param (t_pe:@_) insts ->
            Node (unwords ["Function", name, "->", show t_pe])
                [ paramTree param
                , Node "Body" (toForest insts)
                ]
        where


            paramTree param = if param == Seq.empty
                then Node "No parameters" []
                else Node "Parameters" (toForest param)

data Program = Program Decs deriving (Eq, Show)

instance Treelike Program where
    toTree (Program decs) = Node "Program" (toForest decs)
