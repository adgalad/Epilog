{-# LANGUAGE LambdaCase #-}

module Language.Epilog.AST.Instruction
    ( InstBlock
    , Instruction(..)
    , Type(..)
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.At

import           Data.Foldable              (toList)
import           Data.List                  (intercalate)
import           Data.Sequence              (Seq, empty, fromList, index,
                                             singleton, (<|), (><), (|>))
--------------------------------------------------------------------------------

type InstBlock = Seq (At Instruction)

showInst :: String -> Seq (At Instruction) -> String
showInst sep list = intercalate sep (map (show.item) (toList list))

data Instruction
    = EmptyInst
    | Assign (At Expression) (At Expression)
    | Declaration (At Type) (At String)
    | Initialization (At Type) (At Instruction)
    | If InstBlock
    | Guard (At Expression) InstBlock
    | Finish
    | Return (At Expression)
    deriving (Eq)

instance Show Instruction where
    show = \case
        EmptyInst -> ""
        Assign varid value -> "Assign " ++ show (item varid) ++ " " ++ show (item value)
        Declaration t varid -> show (item t) ++ " " ++ item varid
        Initialization t assign -> show (item t) ++ " (" ++ show (item assign) ++ ")"
        If glist -> "If \n"++ showInst "\n" glist ++ "\nEnd"
        Guard cond inst -> show (item cond) ++ " -> " ++ showInst "\n" inst
        Finish -> "Finish"
        Return value -> "Return " ++ show (item value)
    -- Declaration type id


data Type
    = IntT | CharT | FloatT | BoolT | StringT deriving (Eq, Show)