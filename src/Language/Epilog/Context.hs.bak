{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.Context
    ( ProcSignature (..)
    , verifyExpr
    , verifyLval
    , inst
    , def
    , initialState
    , binaryOperation
    , unaryOperation
    , literal
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Error
import           Language.Epilog.Common
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction hiding (Set)
import qualified Language.Epilog.AST.Instruction as Inst (Set)
import           Language.Epilog.AST.Program
import           Language.Epilog.AST.Type
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable     hiding (empty)
import qualified Language.Epilog.SymbolTable     as ST (lookup)
import           Language.Epilog.Epilog
--------------------------------------------------------------------------------

import           Control.Monad                   (foldM, void, unless)
import qualified Data.Map.Strict                 as Map
import           Data.Sequence                   ((><), (<|), ViewL((:<)))
import qualified Data.Sequence                   as Seq
import Control.Lens (use, (.=), (%=))
--------------------------------------------------------------------------------
-- Context -----------------------------
-- context :: Program -> (SymbolTable, Strings, Types, Procs, Errors)
-- context (Program decs) = (symbols, strings, types, procs, errors)
--     where
--         -- (EpilogState {symbols, strings, pendProcs, procs, types}, e) =
--         (state, errs) = execRWS (mapM_ def decs) () initialState

--         errors' =
--             Seq.sort $ e >< Map.foldrWithKey pendToErrors Seq.empty pendProcs

--         errors = if "main" `Map.member` procs
--             then errors'
--             else NoMain Code <| errors'

--         pendToErrors name ps errs =
--             errs >< foldr (pendToError name) Seq.empty ps

--         pendToError name p errs =
--             errs |> UndefinedProcedure  name p

-- Definitions -------------------------
def :: Definition -> Epilog ()
def GlobalD { gPos, gType, gName, gVal} =
    declaration (Entry gName gType gVal gPos)

def StructD { sPos, sName, sConts {-, sClass -} } = do
    t <- use types
    case sName `Map.lookup` t of
        Just (_, _, p) -> err $ DuplicateDefinition sName p sPos
        Nothing -> do
            st <- foldM cont (emptyP sPos) sConts
            types %= Map.insert sName (userT sName, st, sPos)

def ProcD { pPos, pName, pParams, pType, pInsts } = do
    let signature = ProcSignature pName pType pPos
    procs %= Map.insert pName signature

    openScope' pPos
    sequence_ $ fmap param pParams

    openScope' pPos
    sequence_ $ fmap inst pInsts

    closeScope'
    closeScope'

-- Contents ----------------------------
cont :: SymbolTable -> Content -> Epilog SymbolTable
cont st Content { cPos, cType, cName } = do
    ts <- use types
    case typeName cType `Map.lookup` ts of
        Just _ -> case cName `local` st of
            Right Entry { varType, varPosition } -> do
                err $ DuplicateDeclaration cName varType varPosition cType cPos
                return st
            Left _ -> return $ insertSymbol cName entry st
        Nothing -> do
            err $ UndefinedType (typeName cType) cName cPos
            return st
    where
        entry = Entry cName cType Nothing cPos

-- Parameters --------------------------
param :: Parameter -> Epilog ()
param (Parameter p t name) =
    declaration (Entry name t Nothing p)

-- Instructions ------------------------
inst :: Instruction -> Epilog ()
inst (Declaration p t name val) =
    declaration (Entry name t val p)

inst (Assign p lval rval) = do
    void $ verifyLval lval p
    verifyExpr rval

inst (ICall p name exprs) = do
    verifyCall name p
    sequence_ $ fmap verifyExpr exprs

inst (If _ guards) =
    sequence_ $ fmap guard guards

inst (Case _ expr sets) = do
    verifyExpr expr
    sequence_ $ fmap set sets

inst (For p Nothing var ranges) = do
    verifySymbol var p
    sequence_ $ fmap range ranges

inst (For p (Just t) var ranges) = do
    openScope' p
    declaration (Entry var t Nothing p)
    sequence_ $ fmap range ranges
    closeScope'

inst (While _ guards) =
    sequence_ $ fmap guard guards

inst (Read p lval) =
    void $ verifyLval lval p

inst (Write _ expr) =
    verifyExpr expr

inst (Finish _) = return ()

guard :: Guard -> Epilog ()
guard (p, expr, insts) = do
    verifyExpr expr
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

set :: Inst.Set -> Epilog ()
set (p, exprs, insts) = do
    sequence_ $ fmap verifyExpr exprs
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

range :: Range -> Epilog ()
range (p, from, to, insts) = do
    verifyExpr from
    verifyExpr to
    openScope' p
    sequence_ $ fmap inst insts
    closeScope'

-- Expression --------------------------
verifyExpr :: Expression -> Epilog ()
verifyExpr (Lval      p lval   ) = void $ verifyLval lval p
verifyExpr (Binary    _ _ e0 e1) = verifyExpr e0 >> verifyExpr e1
verifyExpr (Unary     _ _ e0   ) = verifyExpr e0
verifyExpr (LitString p string ) =
    strings %= Map.insertWith (><) string (Seq.singleton p)
verifyExpr (ECall     p n exprs) = do
    verifyCall n p
    sequence_ $ fmap verifyExpr exprs
verifyExpr                     _ = return ()

verifyLval :: Lval -> Position -> Epilog (Maybe Type)
verifyLval (Variable n) p = do
    s <- use symbols
    case n `ST.lookup` s of
        Right Entry {varType} -> return (Just varType)
        Left _ -> do
            err $ OutOfScope n p
            return Nothing
verifyLval (Member lval member) p = do
    t <- verifyLval lval p
    case t of
        Nothing -> return Nothing
        Just type0 -> do
            ts <- use types
            if size type0 /= 0
                then do
                    err $ MemberOfArray member p
                    return Nothing
                else
                    case typeName type0 `Map.lookup` ts of
                        Nothing -> return Nothing
                        Just (_, st, _) -> case member `ST.lookup` st of
                            Left _ -> do
                                err $ InvalidMember member p
                                return Nothing
                            Right Entry { varType } -> return (Just varType)
verifyLval (Index lval expr) p = do
    t <- verifyLval lval p
    verifyExpr expr
    case t of
        Nothing -> return Nothing
        Just type0@(Type name dims) ->
            if size type0 == 0
                then err (InvalidIndex p) >> return Nothing
                else return $ Just (Type name (Seq.drop 1 dims))

verifyCall :: Name -> Position -> Epilog ()
verifyCall proc p = do
    procedures <- use procs
    unless (proc `Map.member` procedures) $
        pendProcs %= Map.insertWith (><) proc (Seq.singleton p)

verifySymbol :: String -> Position -> Epilog ()
verifySymbol name p = do
    s <- use symbols
    unless (isSymbol name s) $
        err $ OutOfScope name p

declaration :: Entry -> Epilog ()
declaration entry@(Entry name t val p) = do
    -- EpilogState { symbols, types } <- get
    symbs <- use symbols
    ts    <- use types
    case name `local` symbs of
        Right Entry {varType, varPosition} ->
            err $ DuplicateDeclaration name varType varPosition t p
        Left _ -> case typeName t `Map.lookup` ts of
            Just _  ->
                symbols %= insertSymbol name entry
            Nothing ->
                err $ UndefinedType (typeName t) name p

    case val of
        Nothing  -> return ()
        Just expr -> verifyExpr expr

openScope' :: Position -> Epilog ()
openScope' p =
    symbols %= openScope p

closeScope' :: Epilog ()
closeScope' =
    symbols %= \st -> case goUp st of
        Left  _   -> st
        Right st' -> st'

binaryOperation :: BinaryOp -> Epilog ()
binaryOperation op = do
    expr <- use expression
    case Seq.viewl expr of
        e2 :< xs -> case Seq.viewl xs of
            e1 :< ys ->
                expression .= Binary (pos e1) op e1 e2 <| ys

unaryOperation :: UnaryOp -> Epilog ()
unaryOperation op =  do
    expr <- use expression
    case Seq.viewl expr of
        e :< xs ->
            expression .= Unary (pos e) op e <| xs

literal :: Expression -> Epilog ()
literal lit = do
    verifyExpr lit
    expression %= (lit <|)
