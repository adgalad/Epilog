{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Epilog.AST.Instruction
    ( Instruction (..)
    , Type (..)
    , Exps
    , Guard
    , Guards
    , Insts
    , Range
    , Ranges
    -- , Set
    -- , Sets
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.Common
import           Language.Epilog.Position
import           Language.Epilog.Treelike
import           Language.Epilog.Type
--------------------------------------------------------------------------------

-- Sequence synonyms -----------------------------------------------------------
type Insts  = Seq Instruction

type Guard  = (Position, Expression, Insts)
type Guards = Seq Guard

-- type Set    = (Position, Exps, Insts)
-- type Sets   = Seq Set

type Range  = (Position, Expression, Expression, Insts)
type Ranges = Seq Range

-- Instructions ----------------------------------------------------------------
data Instruction
  = Assign -- AST built
    { instP        :: Position
    , assignTarget :: Lval
    , assignVal    :: Expression }
  | ICall -- AST built
    { instP       :: Position
    , callName    :: Name
    , callArgs    :: Seq (Either Lval Expression)
    , callRetType :: Type }
  | If -- AST built
    { instP    :: Position
    , ifGuards :: Guards }
  -- | Case -- Removed from language :<
  --     { instP    :: Position
  --     , caseExp  :: Expression
  --     , caseSets :: Sets
  --     }
  | For -- AST built
    { instP     :: Position
    , forVar    :: Lval
    , forRanges :: Ranges }
  | While -- AST built
    { instP       :: Position
    , whileGuards :: Guards }
  | Read -- AST built
    { instP      :: Position
    , readTarget :: Lval }
  | Write -- AST built
    { instP    :: Position
    , writeVal :: Expression }
  | Make -- AST built
    { instP      :: Position
    , makeTarget :: Lval }
  | Ekam -- AST built
    { instP      :: Position
    , ekamTarget :: Lval }
  | Answer -- AST built
    { instP     :: Position
    , answerVal :: Expression }
  | Finish -- AST built
    { instP :: Position }
  | Var
    { instP     :: Position
    , varName   :: Name
    , varOffset :: Offset
    , varSize   :: Word32 }
  deriving (Eq, Show)

instance P Instruction where
  pos = instP

instance Treelike Insts where
  toTree insts = Node "Instructions" (toForest insts)

instance Treelike Instruction where
  toTree = \case
    Assign p lval expr ->
      Node (unwords ["Assign", showP p])
        [toTree lval, toTree expr]

    ICall p proc args _ ->
      Node (unwords ["Instruction Call", proc, showP p])
        [Node "Arguments" (toList . fmap (either toTree toTree) $ args)]

    If p guards ->
      Node (unwords ["If", showP p])
        (toList . fmap guardTree $ guards)

    -- Case p var sets ->
    --     Node (unwords ["Case", showP p]) $
    --         toTree var :
    --         (toList . fmap setTree $ sets)

    For p var ranges ->
      Node (unwords ["For", showP p]) $
        Node "Variable" [toTree var] :
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

    Make p lval ->
      Node (unwords ["Make", showP p])
        [toTree lval]

    Ekam p lval ->
      Node (unwords ["Ekam", showP p])
        [toTree lval]

    Answer p expr ->
      Node (unwords ["Answer", showP p])
        [toTree expr]

    Finish p ->
      leaf (unwords ["Finish", showP p])

    Var p n o s ->
      Node (unwords ["Var", showP p])
        [ Node "name"   [ leaf n ]
        , Node "offset" [ leaf . show $ o ]
        , Node "size"   [ leaf . show $ s ] ]

    where
      guardTree :: Guard -> Tree String
      guardTree (p, cond, insts) =
        Node (unwords ["Guard", showP p])
            [ Node "Condition" [toTree cond]
            , Node "Body" (toForest insts) ]

      -- setTree :: Set -> Tree String
      -- setTree (p, exprs, insts) =
      --   Node (unwords ["Set", showP p])
      --       [ Node "Values" (toForest exprs)
      --       , Node "Body" (toForest insts) ]

      rangeTree :: Range -> Tree String
      rangeTree (p, lower, upper, insts) =
        Node (unwords ["Range", showP p])
            [ Node "From" [toTree lower]
            , Node "To"   [toTree upper]
            , Node "Body" (toForest insts) ]
