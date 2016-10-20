{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Epilog.AST.Expression
  ( Expression (..)
  , Expression' (..)
  , Lval (..)
  , Lval' (..)
  , VarKind (..)
  , BinaryOp (..)
  , UnaryOp (..)
  , Exps
  ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Position
import           Language.Epilog.Treelike
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Data.Foldable            (toList)
import           Data.Int                 (Int32)
import           Data.Semigroup           ((<>))
import           Data.Sequence            (Seq)
import           Data.Tree                (flatten)
import           Data.Word                (Word8)
--------------------------------------------------------------------------------
data VarKind = Global | RefParam | Param | Local deriving (Eq, Ord, Show)

-- Useful synonyms ---------------------
type Exps = Seq Expression
type Name = String

-- Expressions -------------------------
data Expression = Expression
  { expPos  :: Position
  , expType :: Type
  , exp'    :: Expression'}
  deriving (Eq, Show)

data Expression'
  = LitBool   Bool
  | LitChar   Word8
  | LitInt    Int32
  | LitFloat  Float
  | LitString Int32

  -- | Otherwise

  | Rval      Lval

  | ECall     Name     (Seq (Either Lval Expression))

  | Binary    BinaryOp Expression Expression
  | Unary     UnaryOp  Expression
  deriving (Eq, Show)

instance P Expression where
  pos = expPos

instance Treelike Expression where
  toTree Expression { exp' } = case exp' of
    LitBool val ->
      leaf (unwords [if val then "true" else "false"])

    LitChar val ->
      leaf (unwords [show val])

    LitInt val ->
      leaf (unwords [show val])

    LitFloat val ->
      leaf (unwords [show val])

    LitString val ->
      leaf ("String #" <> unwords [show val])

    -- Otherwise ->
    --   leaf (unwords ["otherwise"])

    Rval lval ->
      Node (unwords ["Lval"]) [toTree lval]

    ECall proc args ->
      Node (unwords ["Expression Call", proc])
        [Node "Arguments" (toList . fmap (either toTree toTree) $ args)]

    Binary op exp0 exp1 ->
      Node (unwords [show op]) (toForest [exp0, exp1])

    Unary op expr ->
      Node (unwords [show op]) [toTree expr]

data BinaryOp
  = And | Andalso | Or | Orelse | Xor
  | Band | Bor | Bsl | Bsr | Bxor
  | Plus | Minus | Times | FloatDiv | IntDiv | Rem
  | LTop | LEop | GTop | GEop | EQop | NEop | FAop | NFop
  deriving Eq

instance Show BinaryOp where
  show = \case
    And        -> "and"
    Andalso    -> "andalso"
    Or         -> "or"
    Orelse     -> "orelse"
    Xor        -> "xor"
    Band       -> "band"
    Bor        -> "bor"
    Bsl        -> "bsl"
    Bsr        -> "bsr"
    Bxor       -> "bxor"
    Plus       -> "(+)"
    Minus      -> "(-)"
    Times      -> "(*)"
    FloatDiv   -> "(/)"
    IntDiv     -> "div"
    Rem        -> "rem"
    LTop       -> "(<)"
    LEop       -> "(=<)"
    GTop       -> "(>)"
    GEop       -> "(>=)"
    EQop       -> "(=)"
    NEop       -> "(/=)"
    FAop       -> "(|)"
    NFop       -> "(!|)"

data UnaryOp
  = Not
  | Bnot
  | Uminus
  deriving Eq

instance Show UnaryOp where
  show = \case
    Not         -> "not"
    Bnot        -> "bnot"
    Uminus      -> "(-)"

-- Lval ------------------------------------------------------------------------

data Lval = Lval
  { lvalType :: Type
  , lval'    :: Lval' }
  deriving (Eq, Show)

data Lval'
  = Variable
    { lName   :: Name
    , lKind   :: VarKind
    , lOffset :: Int }
  | Member
    { lInner  :: Lval
    , mName   :: Name
    , mOffset :: Int }
  | Index
    { lInner :: Lval
    , lIdx   :: Expression }
  | Deref
    { lInner :: Lval }
  deriving (Eq, Show)

instance Treelike Lval where
  toTree = aux1 . reverse . aux0
    where
      aux0 Lval { lval' } = case lval' of
        Variable { lName, lKind } ->
          [lName <> "<" <> show lKind <> ">"]
        Member lval member _ ->
          ('_': member) : aux0 lval
        Index lval index ->
          (show . flatten . toTree $ index) : aux0 lval
        Deref lval ->
          "^" : aux0 lval

      aux1 (x:y:xs) =
        Node x [aux1 (y:xs)]
      aux1 [x] =
        leaf x
      aux1 [] =
        leaf ""
