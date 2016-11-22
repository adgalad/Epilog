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
  { parName   :: Name
  , parType   :: Type
  , parOffset :: Offset
  , parSize   :: Size
  , parPos    :: Position
  , parRef    :: Bool }
  deriving (Eq, Show)

instance Treelike Parameter where
  toTree Parameter { parPos, parType, parName } =
    Node (unwords [parName, showP parPos]) [toTree parType]
--------------------------------------------------------------------------------

data Procedure
  = Procedure
    { procName       :: Name
    , procPos        :: Position
    , procType       :: Type
    , procParams     :: Params
    , procDef        :: Maybe (IBlock, Scope)
    , procStackSize  :: Word32
    , procParamsSize :: Word32}
  | EpiProc
    { procName      :: Name
    , procType      :: Type }
  deriving (Eq)

instance Treelike Procedure where
  toTree Procedure { procName, procPos, procType, procParams, procDef, procStackSize, procParamsSize } =
    Node ("`" <> procName <> "`") $
      leaf ("Declared " <> show procPos) :
      leaf ("Type: " <> show procType) :
      leaf ("Stack Size: " <> show procStackSize) :
      leaf ("Params Size: " <> show procParamsSize) :
      Node "Parameters" (toForest procParams) :
      case procDef of
        Nothing -> []
        Just (block, scope) ->
          [ Node "Scope" [toTree scope]
          , Node "Instructions" [toTree block] ]
  toTree EpiProc { procName, procType } =
    Node ("Native procedure `" <> procName <> "`")
      [leaf ("Type: " <> show procType)]
