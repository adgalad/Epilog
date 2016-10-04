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
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (.=))
--------------------------------------------------------------------------------

irProcedure :: Procedure -> IRMonad ()
irProcedure Procedure { procName, procPos {-, procType-}
                      {-, procParams-}, procDef } =
  case procDef of
    Nothing -> liftIO . putStrLn $ "Epilog native procedure `" <> procName <> "`"
    Just (insts, scope) -> case procName of
      "main" -> do
        g <- use global
        let smbs = (\(Right x) -> x) . goDownFirst . insertST scope . focus $ g
        symbols .= smbs

        exit <- newLabel
        nextBlock <|= exit

        newLabel >>= (#)
        addTAC . Comment $ "Main procedure at " <> showP procPos
        mapM_ irInstruction insts

        (exit #)
        terminate Exit

      _ -> liftIO . putStrLn $ "User-defined procedure `" <> procName <> "`"
