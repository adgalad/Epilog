{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.IR.Program
  ( irProgram
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Program
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.Procedure (irProcedure)
import qualified Language.Epilog.IR.TAC       as TAC (Program (..))
--------------------------------------------------------------------------------
import           Control.Lens                 (use, (.=))
--------------------------------------------------------------------------------

irProgram :: Program -> IRMonad TAC.Program
irProgram Program { procs, scope {-, strings-} } = do
  global .= scope
  mapM_ irProcedure procs
  TAC.Program <$> use dataSegment <*> use modules
