{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Language.Epilog.AST.AST
    ( insertExpr
    , topExpr
    , insertInst
    , topInsts
    , guard
    , range
    , insert
    , binOp
    , unaryOp
    , assign
    , openScope
    , buildIf
    , buildFor
    ) where 
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Expression
import           Language.Epilog.At
import           Language.Epilog.Epilog
import           Language.Epilog.Treelike
--------------------------------------------------------------------------------
import           Data.Sequence          (Seq, (|>), (<|), viewl
                                        , ViewL ((:<)))
import qualified Data.Sequence          as Seq (empty)
import           Control.Lens           (use, (%=), (.=))
import           Data.Foldable          (toList)
--------------------------------------------------------------------------------


insertExpr :: Expression -> Epilog ()
insertExpr expr = expression %= ( expr <|)

topExpr :: Epilog Expression
topExpr = do
    expr <- use expression
    case viewl expr of
        x :< xs -> do 
            expression .= xs
            return x
        _       -> undefined

insertInst :: Instruction -> Epilog ()
insertInst inst = instructions %= (\(x:xs) -> (x|>inst):xs)

openScope :: Epilog ()
openScope = instructions %= (Seq.empty:) 

topInsts :: Epilog (Insts)
topInsts = do 
    insts <- use instructions
    case insts of 
        (x:xs) -> do
            instructions .= xs
            return x 
        _      -> undefined 


guard :: Position -> Epilog ()
guard p = do 
    e <- topExpr 
    insts <- topInsts
    guards %= (|> (p, e, insts))

range :: Position -> Epilog ()
range p =  do 
    h <- topExpr 
    l <- topExpr 
    insts <- topInsts 
    ranges %= (|> (p, l, h, insts))

buildFor :: Position -> Bool -> Epilog () 
buildFor p decl = do 
    vars <- use forVars
    let (var :@ _, t) = head vars
    ranges' <- use ranges
    if decl 
        then insertInst $ For p (Just t) var ranges' 
        else insertInst $ For p  Nothing var ranges' 
    ranges .= Seq.empty

buildIf :: Position -> Epilog () 
buildIf p = do 
    guards' <- use guards
    insertInst $ If p guards'
    guards .= Seq.empty

insert :: Instruction -> Epilog ()
insert inst = ast %= (|> inst)

binOp :: BinaryOp -> Position -> Epilog ()
binOp op p = do
    e2 <- topExpr
    e1 <- topExpr
    insertExpr $ (Binary p op e1 e2)

unaryOp :: UnaryOp -> Position -> Epilog ()
unaryOp op p = do
    e <- topExpr
    insertExpr $ Unary p op e

assign :: Epilog ()
assign = do
    expr <- topExpr
    lval <- topExpr
    case lval of 
        Lval p elval ->
             insertInst $ (Assign p elval expr) 
        _           -> undefined