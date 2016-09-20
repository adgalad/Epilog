{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Program
  ( Program (..)
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Procedure
import           Language.Epilog.Epilog        (Types)
import           Language.Epilog.SymbolTable
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Prelude                       hiding (Either, null)
--------------------------------------------------------------------------------

data Program = Program
  { types :: Types
  , procs :: Procedures
  , scope :: Scope }

instance Treelike Program where
    toTree Program { types, procs, scope} =
      Node "Program"
        [ Node "Types" (toForest' $ fst <$> types)
        , Node "Procs" (toForest procs)
        , Node "Scope" [toTree scope]]
