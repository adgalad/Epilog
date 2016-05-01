{-# LANGUAGE LambdaCase        #-}

module Language.Epilog.Instruction 
	( InstBlock
	 ,Instruction(..)
     , Method(..)
	)
where 

import           Language.Epilog.Lexeme
import           Language.Epilog.Expression
import           Data.List                 (intercalate) 
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq, empty, fromList, index,
                                            singleton, (><), (|>), (<|))

type InstBlock = Seq (Lexeme Instruction)

showInst :: String -> Seq (Lexeme Instruction) -> String
showInst sep list = intercalate sep (map (show.token) (toList list))

data Method
    = Proc (Lexeme String) InstBlock InstBlock
    | Func (Lexeme String) InstBlock (Lexeme Type) InstBlock

instance Show Method where
    show = \case 
        Proc name param inst -> "Proc " ++ token name ++ " ( "++ showInst ", " param ++ ")\n" ++ showInst "\n" inst
        Func name param t inst -> "Func " ++ token name ++ " ( "++ showInst ", " param ++ ")" ++ "->" ++ show (token t) ++"\n"++ showInst "\n" inst

data Instruction
    = EmptyInst
    | Assign (Lexeme Expression) (Lexeme Expression)
    | Declaration (Lexeme Type) (Lexeme String)
    | Initialization (Lexeme Type) (Lexeme Instruction)
    | If InstBlock
    | Guard (Lexeme Expression) (InstBlock)
    | Finish
    | Return (Lexeme Expression)
    deriving (Eq)

instance Show Instruction where 
    show = \case  
    	EmptyInst -> ""
        Assign varid value -> "Assign "++ show (token varid) ++ " " ++ show (token value)
        Declaration t varid -> show (token t) ++" "++ (token varid)
        Initialization t assign -> show (token t) ++ " (" ++ show (token assign) ++ ")"
        If glist -> "If \n"++ showInst "\n" glist ++ "\nEnd"
        Guard cond inst -> show (token cond) ++ " -> " ++ showInst "\n" inst
        Finish -> "Finish"
        Return value -> "Return " ++ show (token value)
    -- Declaration type id
