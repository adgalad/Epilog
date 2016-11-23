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
import           Language.Epilog.IR.TAC         hiding (TAC (Var))
import qualified Language.Epilog.IR.TAC         as TAC (TAC (Var))
import           Language.Epilog.Position       hiding (Position (Epilog))
import           Language.Epilog.SymbolTable
import           Language.Epilog.Type           (Type (..), Atom (..), voidT)
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (.=), (<~))
--------------------------------------------------------------------------------

irProcedure :: Procedure -> IRMonad ()
irProcedure Procedure { procName, procPos, procType = _ :-> retType
                      , procParams, procDef, procStackSize } =
  case procDef of
    Nothing -> liftIO . putStrLn $ "Epilog native procedure `" <> procName <> "`"
    Just (iblock, scope) -> do
      g <- use global
      let smbs = (\(Right x) -> x) . goDownFirst . insertST scope . focus $ g
      symbols .= smbs

      retLabel <~ Just <$> newLabel "Return"

      newLabel ("proc_" <> procName) >>= (#)
      addTAC . Comment $ "Procedure at " <> showP procPos
      addTAC $ Prolog procStackSize

      forM_ procParams $
        \Parameter { parName, parOffset, parSize, parRef } -> do
          parName' <- insertVar parName
          addTAC $ TAC.Var parRef parName' (parOffset + 12) parSize

      irIBlock iblock

      use currentBlock >>= \case
        Nothing -> pure ()
        Just _  -> do
          unless (retType == voidT) $ do
            addTAC . Answer $ case retType of
              Basic { atom } -> case atom of
                EpInteger    -> C (IC 0)
                EpBoolean    -> C (BC False)
                EpFloat      -> C (FC 0.0)
                EpCharacter  -> C (CC 0)
                t -> internal $ "bad return type " <> show t
              Pointer {}     -> C (IC 0)
              t -> internal $ "bad return type " <> show t
          use retLabel >>= \case
            Nothing -> internal "nowhere to return"
            Just lbl -> terminate $ Br lbl

      use retLabel >>= \case
        Nothing  -> internal "no return label"
        Just lbl -> (lbl #)
      addTAC . Comment $ "Epilog for procedure " <> procName
      addTAC $ Epilog procStackSize
      terminate Return

      retLabel .= Nothing

      closeModule procName

irProcedure _ = pure ()
