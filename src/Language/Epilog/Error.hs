{-# LANGUAGE LambdaCase #-}

module Language.Epilog.Error
    ( EpilogError (..)
    , Errors
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.Type hiding (name)
import           Language.Epilog.Common
import           Language.Epilog.Position
import           Language.Epilog.Token
import           Language.Epilog.AST.Expression
--------------------------------------------------------------------------------
import           Data.Function            (on)
import           Data.Sequence            (Seq)
import           Data.Foldable            (toList)
--------------------------------------------------------------------------------
type Errors = Seq EpilogError

-- Error Type --------------------------
data EpilogError
    = DuplicateDefinition
        { dDefName :: Name
        , dDefFstP :: Position
        , dDefSndP :: Position
        }
    | OutOfScope
        { oosName :: Name
        , oosP    :: Position
        }
    | DuplicateDeclaration
        { dDecName :: Name
        , dDecFstT :: Type
        , dDecFstP :: Position
        , dDecSndT :: Type
        , dDecSndP :: Position
        }
    | DuplicateField
        { dFldStructName :: Name
        , dFldStructKind :: StructKind
        , dFldStructPos  :: Position
        , dFldName       :: Name
        , dFldFstT       :: Type
        , dFldFstP       :: Position
        , dFldSndT       :: Type
        , dFldSndP       :: Position
        }
    | UndefinedType
        { utTName :: Name
        , utP     :: Position
        }
    | UndefinedProcedure
        { upName :: Name
        , upP    :: Position
        }
    | BadCall
        { bcName  :: Name
        , bcArgs  :: Seq Type
        , bcEArgs :: Seq Type
        , bcP     :: Position
        }
    | InvalidMember
        { imName :: Name
        , imT    :: String
        , imP    :: Position
        }
    | MemberOfArray
        { moaName :: Name
        , moaP    :: Position
        }
    | InvalidAssign
        { iaFstT :: Type
        , iaSndT :: Type
        , iaP    :: Position
        }
    | InvalidGuard
        { icT :: String
        , icP :: Position
        }
    | InvalidRange
        { irFstT :: String
        , irSndT :: String
        , irP    :: Position
        }
    | InvalidArray
        { iaP :: Position }
    | InvalidIndex
        { iiT :: String
        , iiP :: Position
        }
    | NoMain
        { nmP :: Position }
    | LexicalError
        { leP :: Position }
    | Overflow
        { oMsg :: String
        , oP   :: Position
        }
    | RecursiveType
        { rtType :: String
        , rtName :: String
        , rtP    :: Position
        }
    | UnclosedComment
    | Underflow
        { uMsg :: String
        , uP   :: Position
        }
    | UnclosedStringLit
        { uslVal :: String
        , uslP   :: Position
        }
    | UnexpectedToken
        { utT :: Token
        , utP :: Position
        }
    | BadBinaryExpression
        { bbeOp  :: BinaryOp
        , bbeTS  ::  (Type, Type)
        , bbeETS :: [(Type, Type)]
        , bbeP   :: Position
        }
    | BadUnaryExpression
        { bueOp  :: UnaryOp
        , bueT   ::  Type
        , bueETS :: [Type]
        , bueP   :: Position
        }
    deriving (Eq)

instance P EpilogError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        DuplicateField _ _ _ _ _ _ _ p -> p
        InvalidAssign            _ _ p -> p
        InvalidArray                 p -> p
        InvalidIndex               _ p -> p
        InvalidMember            _ _ p -> p
        InvalidGuard               _ p -> p
        InvalidRange             _ _ p -> p
        LexicalError                 p -> p
        MemberOfArray              _ p -> p
        NoMain                       p -> p
        OutOfScope                 _ p -> p
        Overflow                   _ p -> p
        RecursiveType            _ _ p -> p
        UnclosedComment                -> Code
        UnclosedStringLit          _ p -> p
        UndefinedProcedure         _ p -> p
        BadCall                _ _ _ p -> p
        UndefinedType              _ p -> p
        Underflow                  _ p -> p
        UnexpectedToken            _ p -> p
        BadBinaryExpression    _ _ _ p -> p
        BadUnaryExpression     _ _ _ p -> p

instance Ord EpilogError where
    compare = compare `on` pos

instance Show EpilogError where
    show = \case
        DuplicateDeclaration name fstT fstP sndT sndP ->
            "Duplicate declaration " ++ showP sndP ++ ", variable `" ++
            name ++ "` already declared as `" ++ show fstT ++ "` " ++
            showP fstP ++ " cannot be redeclared as `" ++ show sndT ++ "`"

        DuplicateField sName sKind sPos name fstT fstP sndT sndP ->
            "Duplicate field " ++ showP sndP ++ ", field `" ++
            name ++ "` already used as `" ++ show fstT ++ "` " ++
            showP fstP ++ " in " ++ show sKind ++ " `" ++ sName ++ "` " ++
            showP sPos ++ " cannot be redeclared as `" ++ show sndT ++ "`"

        DuplicateDefinition name fstP sndP ->
            "Duplicate definition " ++ showP sndP ++ ": `" ++ name ++
            "` already defined " ++ showP fstP

        InvalidAssign t1 t2 p->
            "Attempted to assign an expression of type `" ++ show t2 ++
            "` to an lval of type `" ++ show t1 ++ "` " ++ showP p

        InvalidArray p ->
            "Index of non-array variable " ++ showP p

        InvalidIndex t p ->
            "Attempted to use an index of type `" ++ t ++ "` " ++ showP p

        InvalidMember member t p ->
            "Not member named `" ++ member ++ "` "++ showP p ++
            " in type `" ++ t ++ "`"

        InvalidGuard t p ->
            "Guards most be of type `boolean`. Actual type is `"++ t ++"` " ++ showP p

        InvalidRange t1 t2 p ->
            "Invalid range at " ++ showP p ++". Lower bound is `" ++ t1 ++
            "` and higher bound is `" ++ t2 ++"`"

        MemberOfArray member p ->
            "Expected array index instead of member " ++ showP p ++
            " member named `" ++ member ++ "`"

        NoMain _ ->
            "No procedure `main` defined in the program"

        LexicalError (Position (line, column)) ->
            "Lexical error at line " ++ show line ++
            ", column " ++ show column

        LexicalError _ -> undefined

        UnclosedComment ->
            "Comment not closed at end of file"

        OutOfScope name p ->
            "Out of scope variable " ++ showP p ++ ": `" ++ name ++
            "` not available in this scope."

        Overflow msg p ->
            "Overflow \n" ++ msg ++ "\n" ++ show p

        RecursiveType t name p ->
            "Attempted to declare a recursive field named `" ++ name ++
            "` " ++ showP p ++" in struct `" ++ t ++ "`"

        UndefinedType nameT p ->
            "Attempted to declare variable of undefined type " ++ showP p ++
            ", type `" ++ nameT ++ "`"

        UndefinedProcedure name p ->
            "Call to undeclared procedure " ++ showP p ++ " named `" ++
            name ++ "`"

        BadCall name args expArgs p ->
            "Bad call to procedure " ++ showP p ++ " named `" ++
            name ++ "`, expected args of types " ++ show (toList expArgs) ++
            ", but instead received " ++ show (toList args)

        Underflow msg p ->
            "Underflow \n" ++ msg ++ "\n" ++ show p

        UnclosedStringLit val p ->
            "Unclosed String Literal \n" ++ val ++ "\n" ++ show p

        UnexpectedToken t p ->
            "Unexpected token \n" ++ show t ++ "\n" ++ show p

        BadBinaryExpression op ts ets p ->
            "Bad binary expression " ++ showP p ++ ", operator `" ++
            show op ++ "` expected one pair of " ++ show ets ++
            ", but received " ++ show ts

        BadUnaryExpression op t ets p ->
            "Bad unary expression " ++ showP p ++ ", operator `" ++
            show op ++ "` expected one of " ++ show ets ++
            ", but received `" ++ show t ++ "`"
