{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Epilog.IR.Procedure
  ( irProcedure
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Procedure
import           Language.Epilog.Common
import           Language.Epilog.IR.Instruction
import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Language.Epilog.Position       hiding (Position (Epilog))
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (.=))
--------------------------------------------------------------------------------

irProcedure :: Procedure -> IRMonad ()
irProcedure Procedure { procName, procPos {-, procType-}
                      {-, procParams-}, procDef, procStackSize } =
  case procDef of
    Nothing -> liftIO . putStrLn $ "Epilog native procedure `" <> procName <> "`"
    Just (insts, scope) -> do
      g <- use global
      let smbs = (\(Right x) -> x) . goDownFirst . insertST scope . focus $ g
      symbols .= smbs

      newLabel >>= (#)
      addTAC . Comment $ "Procedure at " <> showP procPos
      addTAC $ Prelude procStackSize

      mapM_ irInstruction insts

      addTAC $ Epilog procStackSize

      case procName of
        "main" -> terminate Exit
        _      -> use currentBlock >>= \case
          Nothing -> pure ()
          Just _  -> terminate $ Return Nothing

      closeModule procName

irProcedure _ = pure ()
