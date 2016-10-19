{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.IR.Program
  ( irProgram
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Program
import           Language.Epilog.Common
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.Procedure (irProcedure)
import           Language.Epilog.IR.TAC       (Data (..))
import qualified Language.Epilog.IR.TAC       as TAC (Program (..))
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (.=))
import qualified Data.Map                     as Map (toList)
--------------------------------------------------------------------------------

irProgram :: Program -> IRMonad TAC.Program
irProgram Program { procs, scope, strings } = do
  global .= scope

  forM_ (Map.toList strings) $ \(str, idx) ->
    dataSegment |>= StringData ("_str" <> show idx) str

  mapM_ irProcedure procs

  TAC.Program <$> use dataSegment <*> use modules
