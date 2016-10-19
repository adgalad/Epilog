{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.IR.Program
  ( irProgram
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Program
import           Language.Epilog.Common
import           Language.Epilog.IR.Expression
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.Procedure  (irProcedure)
import           Language.Epilog.IR.TAC        (Data (..), Operand (R),
                                                TAC (..), Terminator (..))
import qualified Language.Epilog.IR.TAC        as TAC (Program (..))
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type          (sizeT)
--------------------------------------------------------------------------------
import           Control.Lens                  (use, (.=))
import qualified Data.Map                      as Map (toList)
--------------------------------------------------------------------------------

irProgram :: Program -> IRMonad TAC.Program
irProgram Program { procs, scope, strings } = do
  global .= scope

  forM_ (Map.toList strings) $ \(str, idx) ->
    dataSegment |>= StringData ("_str" <> show idx) str

  newLabel >>= (#)
  forM_ (sEntries scope) $ \Entry { eName, eType, eInitialValue } -> do
    dataSegment |>= VarData
      { dName  = eName
      , dSpace = fromIntegral $ sizeT eType }
    case eInitialValue of
      Nothing -> pure ()
      Just e  -> do
        t <- irExpression e
        addTAC $ R eName :*= t
  addTAC $ Call "main"
  terminate $ Exit
  closeModule "_entry"

  mapM_ irProcedure procs

  TAC.Program <$> use dataSegment <*> use modules
