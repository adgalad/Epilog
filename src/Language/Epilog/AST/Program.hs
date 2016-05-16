{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Program
    ( Content (..)
    , Definition (..)
    , Parameter (..)
    , Program (..)
    , StructClass (..)
    , Conts
    , Defs
    , Params
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Position
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Sequence                   (Seq, null)
import           Prelude                         hiding (null, Either)
--------------------------------------------------------------------------------
-- Synonyms --------------------------------------------------------------------
type Name   = String
type Params = Seq Parameter
type Conts  = Seq Content

-- Other types -----------------------------------------------------------------
data StructClass = Record | Either
                 deriving (Eq, Ord, Show, Read)

data Parameter = Parameter
    { parPos  :: Position
    , parType :: Type
    , parName :: Name
    }
    deriving (Eq, Show)

instance Treelike Parameter where
    toTree Parameter { parPos, parType, parName } =
        Node (unwords [parName, showP parPos]) [toTree parType]

data Content = Content
    { cPos  :: Position
    , cType :: Type
    , cName :: Name
    }
    deriving (Eq, Show)

instance Treelike Content where
    toTree Content { cPos, cName, cType } =
        Node (unwords [cName, showP cPos]) [toTree cType]

-- Top Level Definitions -------------------------------------------------------
data Definition
    = GlobalD
        { gPosition :: Position
        , gVar      :: Instruction
        }
    | StructD
        { sPosition :: Position
        , sName     :: Name
        , sClass    :: StructClass
        , sContents :: Conts
        }
    | ProcD
        { pPosition :: Position
        , pName     :: Name
        , pParams   :: Params
        , pInsts    :: Insts
        }
    deriving (Eq, Show)

instance P Definition where
    pos = \case
        GlobalD p _     -> p
        StructD p _ _ _ -> p
        ProcD   p _ _ _ -> p

instance Treelike Definition where
    toTree = \case
        GlobalD _ var ->
            toTree var

        StructD p name Record insts ->
            Node
                (unwords ["Record", name, showP p])
                [Node "Entries" (toForest insts)]

        StructD p name Either insts ->
            Node
                (unwords ["Either", name, showP p])
                [Node "Members" (toForest insts)]

        ProcD p name params insts ->
            Node (unwords ["Procedure", name, showP p])
                [ if null params
                    then Node "No parameters" []
                    else Node "Parameters" (toForest params)
                , Node "Body" (toForest insts)
                ]

-- Program ---------------------------------------------------------------------

type Defs = Seq Definition

data Program = Program Defs deriving (Eq, Show)

instance Treelike Program where
    toTree (Program decs) = Node "Program" (toForest decs)
