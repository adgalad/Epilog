{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Program
  ( Program (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Procedure
import           Language.Epilog.Epilog        (Strings, Types)
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import qualified Data.Map                      as Map (keys)
import           Prelude                       hiding (Either, null)
--------------------------------------------------------------------------------

data Program = Program
  { types   :: Types
  , procs   :: Procedures
  , scope   :: Scope
  , strings :: Strings }

instance Treelike Program where
    toTree Program { types, procs, scope, strings } =
      Node "Program"
        [ Node "Types"   (toForest' $ fst <$> types)
        , Node "Procs"   (toForest procs)
        , Node "Scope"   [toTree scope]
        , Node "Strings" (leaf <$> Map.keys strings) ]
