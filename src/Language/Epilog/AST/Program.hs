{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Program
    ( Program (..)
    , Declaration (..)
    , Dec
    , Decs
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.At
import           Language.Epilog.Treelike

import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq (empty)
--------------------------------------------------------------------------------

type Dec = At Declaration
type Decs = Seq Dec

data Declaration
    = GlobalD
    | EitherD
    | RecordD
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
        GlobalD -> Node undefined []
        EitherD -> Node undefined []
        RecordD -> Node undefined []
        ProcD (name :@_) vars insts ->
            Node (unwords ["Procedure", name])
                [ varsTree vars
                , Node "Body" (toForest insts)
                ]
        FunD  (name :@_) vars (t_pe:@_) insts ->
            Node (unwords ["Function", name, "->", show t_pe])
                [ varsTree vars
                , Node "Body" (toForest insts)
                ]
        where
            varsTree vars = if vars == Seq.empty
                then Node "No vars" []
                else Node "Vars" (toForest vars)

data Program = Program Decs deriving (Eq, Show)

instance Treelike Program where
    toTree (Program decs) = Node "Program" (toForest decs)
