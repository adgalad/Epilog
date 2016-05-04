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

import           Data.Sequence                   (Seq, null)
import           Prelude                         hiding (null)
--------------------------------------------------------------------------------
-- Top Level Declarations ------------------------------------------------------

data Declaration
    = GlobalD Inst
    | RecordD
        { recordName    :: At String
        , recordEntries :: Insts
        }
    | EitherD
        { eitherName    :: At String
        , eitherMembers :: Insts
        }
    | ProcD
        { procName   :: At String
        , procParams :: Insts
        , procInsts  :: Insts
        }
    deriving (Eq, Show)

instance Treelike Declaration where
    toTree = \case
        GlobalD var ->
            Node "Global" [toTree var]

        RecordD (name :@ _ ) insts ->
            Node ("Record " ++ name) [Node "Entries" (toForest insts)]

        EitherD (name :@ _ ) insts ->
            Node ("Either " ++ name) [Node "Members" (toForest insts)]

        ProcD (name :@ _ ) params insts ->
            Node ("Procedure " ++ name)
                [ if null params
                    then Node "No parameters" []
                    else Node "Parameters" (toForest params)
                , Node "Body" (toForest insts)
                ]

-- Program ---------------------------------------------------------------------

type Dec = At Declaration
type Decs = Seq Dec

data Program = Program Decs deriving (Eq, Show)

instance Treelike Program where
    toTree (Program decs) = Node "Program" (toForest decs)
