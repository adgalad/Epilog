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
import           Language.Epilog.Type           (Type (..), Atom (..))
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (.=), (<~))
--------------------------------------------------------------------------------

irProcedure :: Procedure -> IRMonad ()
irProcedure Procedure { procName, procPos, procType = _ :-> retType
                      {-, procParams-}, procDef, procStackSize } =
  case procDef of
    Nothing -> liftIO . putStrLn $ "Epilog native procedure `" <> procName <> "`"
    Just (insts, scope) -> do
      g <- use global
      let smbs = (\(Right x) -> x) . goDownFirst . insertST scope . focus $ g
      symbols .= smbs

      retLabel <~ Just <$> newLabel
      when (retType /= EpVoid) $
        retTemp <~ Just <$> newTemp

      newLabel >>= (#)
      addTAC . Comment $ "Procedure at " <> showP procPos
      addTAC $ Prelude procStackSize

      mapM_ irInstruction insts

      use currentBlock >>= \case
        Nothing -> pure ()
        Just _  -> do
          use retTemp >>= \case
            Nothing -> pure ()
            Just t  -> do
              addTAC . (t :=) . Id $ case retType of
                Basic { atom } -> case atom of
                  EpInteger    -> C (IC 0)
                  EpBoolean    -> C (BC False)
                  EpFloat      -> C (FC 0.0)
                  EpCharacter  -> C (CC 0)
                Pointer {}     -> C (IC 0)
                _ -> internal $ "bad return type " <> show t
          use retLabel >>= \case
            Nothing -> internal "nowhere to return"
            Just lbl -> terminate $ Br lbl

      use retLabel >>= \case
        Nothing  -> internal "no return label"
        Just lbl -> (lbl #)
      addTAC $ Epilog procStackSize
      use retTemp >>= terminate . Return

      retLabel .= Nothing
      retTemp .= Nothing

      closeModule procName

irProcedure _ = pure ()
