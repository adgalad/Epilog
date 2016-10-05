{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.IR.Program
  ( irProgram
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Program
import           Language.Epilog.Common
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.Procedure (irProcedure)
import           Language.Epilog.IR.TAC
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (.=))
--------------------------------------------------------------------------------

irProgram :: Program -> IRMonad (Map Label Block)
irProgram Program { procs, scope } = do
  global .= scope
  -- types .= types
  mapM_ irProcedure procs
  use blocks
