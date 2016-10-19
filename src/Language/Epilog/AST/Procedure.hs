{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Procedure
  ( Procedure (..)
  , Procedures
  , Parameter (..)
  , Params
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Common
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------

type Procedures = Map String Procedure
type Params = Seq Parameter
--------------------------------------------------------------------------------

data Parameter = Parameter
  { parName :: Name
  , parType :: Type
  , parPos  :: Position }
  deriving (Eq, Show)

instance Treelike Parameter where
  toTree Parameter { parPos, parType, parName } =
    Node (unwords [parName, showP parPos]) [toTree parType]
--------------------------------------------------------------------------------

data Procedure
  = Procedure
    { procName      :: Name
    , procPos       :: Position
    , procType      :: Type
    , procParams    :: Params
    , procDef       :: Maybe (Insts, Scope)
    , procStackSize :: Word32 }
  | EpiProc
    { procName      :: Name
    , procType      :: Type }
  deriving (Eq)

instance Treelike Procedure where
  toTree Procedure { procName, procPos, procType, procParams, procDef, procStackSize } =
    Node ("`" <> procName <> "`") $
      leaf ("Declared " <> show procPos) :
      leaf ("Type: " <> show procType) :
      leaf ("Stack Size: " <> show procStackSize) :
      Node "Parameters" (toForest procParams) :
      case procDef of
        Nothing -> []
        Just (insts, scope) ->
          [ Node "Scope" [toTree scope]
          , Node "Insts" [toTree insts] ]
  toTree EpiProc { procName, procType } =
    Node ("Native procedure `" <> procName <> "`")
      [leaf ("Type: " <> show procType)]
