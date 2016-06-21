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
import           Data.Int                 (Int32)
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
    | BadRead
        { brType  :: Type
        , brP     :: Position
        }
    | BadWrite
        { bwType  :: Type
        , bwP     :: Position
        }
    | BadFinish
        { bfERet  :: Type
        , bfProcP :: Position
        , bfRetP  :: Position
        }
    | BadAnswer
        { baERet  :: Type
        , baARet  :: Type
        , baProcP :: Position
        , baRetP  :: Position
        }
    | InvalidField
        { ifName :: Name
        , ifT    :: Name
        , ifP    :: Position
        }
    | InvalidMember
        { imName :: Name
        , imT    :: Name
        , imP    :: Position
        }
    | InvalidAccess
        { iaName :: Name
        , iaT    :: Name
        , iaP    :: Position
        }
    | MemberOfArray
        { moaName :: Name
        , moaP    :: Position
        }
    | AssignMismatch
        { iaFstT :: Type
        , iaSndT :: Type
        , iaP    :: Position
        }
    | NonBasicAssign
        { nbaT :: Type
        , nbaP :: Position
        }
    | InvalidGuard
        { igT :: Type
        , igP :: Position
        }
    | InvalidRange
        { irVName :: Name
        , irVType :: Type
        , irVPos  :: Position
        , irFstT  :: Type
        , irSndT  :: Type
        , irP     :: Position
        }
    | InvalidArray
        { iaP :: Position }
    | InvalidSubindex
        { iiT :: Type
        , iiP :: Position
        }
    | NoMain
        { nmP :: Position }
    | LexicalError
        { leP :: Position }
    | RecursiveType
        { rtType :: String
        , rtName :: String
        , rtP    :: Position
        }
    | UnclosedComment
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
    | BadForVar
        { bfvN  :: Name
        , bfvT  :: Type
        , bfvDP :: Position
        , bfvUP :: Position
        }
    | BadCaseExp
        { bceT :: Type
        , bceP :: Position
        }
    | BadCaseIntElem
        { bcieEP :: Position
        , bciVal :: Char
        , bciVP  :: Position
        }
    | BadCaseCharElem
        { bcceEP :: Position
        , bccVal :: Int32
        , bccVP  :: Position
        }
    | BadDeref
        { bdT    :: Type
        , bdP    :: Position
        }
    deriving (Eq)

instance P EpilogError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        DuplicateField _ _ _ _ _ _ _ p -> p
        AssignMismatch           _ _ p -> p
        NonBasicAssign             _ p -> p
        InvalidArray                 p -> p
        InvalidSubindex            _ p -> p
        InvalidField             _ _ p -> p
        InvalidMember            _ _ p -> p
        InvalidAccess            _ _ p -> p
        InvalidGuard               _ p -> p
        InvalidRange       _ _ _ _ _ p -> p
        LexicalError                 p -> p
        MemberOfArray              _ p -> p
        NoMain                       p -> p
        OutOfScope                 _ p -> p
        RecursiveType            _ _ p -> p
        UnclosedComment                -> Code
        UndefinedProcedure         _ p -> p
        BadCall                _ _ _ p -> p
        BadRead                    _ p -> p
        BadWrite                   _ p -> p
        BadFinish                _ _ p -> p
        BadAnswer              _ _ _ p -> p
        UndefinedType              _ p -> p
        UnexpectedToken            _ p -> p
        BadBinaryExpression    _ _ _ p -> p
        BadUnaryExpression     _ _ _ p -> p
        BadForVar              _ _ _ p -> p
        BadCaseExp                 _ p -> p
        BadCaseIntElem           _ _ p -> p
        BadCaseCharElem          _ _ p -> p
        BadDeref                   _ p -> p

instance Ord EpilogError where
    compare = compare `on` pos

instance Show EpilogError where
    show = \case
        DuplicateDeclaration name fstT fstP sndT sndP ->
            "Duplicate declaration " ++ show sndP ++ ", variable `" ++
            name ++ "` already declared as `" ++ show fstT ++ "` " ++
            show fstP ++ ", cannot be redeclared as `" ++ show sndT ++ "`"

        DuplicateField sName sKind sPos name fstT fstP sndT sndP ->
            "Duplicate field " ++ show sndP ++ ", field `" ++
            name ++ "` already used as `" ++ show fstT ++ "` " ++
            show fstP ++ " in " ++ show sKind ++ " `" ++ sName ++ "` " ++
            show sPos ++ " cannot be redeclared as `" ++ show sndT ++ "`"

        DuplicateDefinition name fstP sndP ->
            "Duplicate definition " ++ show sndP ++ ", `" ++ name ++
            "` already defined " ++ show fstP

        AssignMismatch t1 t2 p->
            "Invalid assignment " ++ show p ++
            ", attempted to assign an expression of type `" ++ show t2 ++
            "` to an lval of type `" ++ show t1 ++ "` "

        NonBasicAssign t1 p->
            "Invalid assignment " ++ show p ++
            ", attempted to assign a value to an lval of type `" ++
            show t1 ++ "`, but only characters, booleans, integers, " ++
            "floats, or pointers may be targets of an assignment."

        InvalidArray p ->
            "Index of non-array variable " ++ show p

        InvalidSubindex t p ->
            "Invalid index " ++ show p ++
            ", attempted to use an expression of type `" ++ show t ++
            "` as a subindex"

        InvalidField field t p ->
            "No field named `" ++ field ++ "` " ++ show p ++
            " in record `" ++ t ++ "`"

        InvalidMember member t p ->
            "No member named `" ++ member ++ "` "++ show p ++
            " in either `" ++ t ++ "`"

        InvalidAccess access t p ->
            "Invalid access `" ++ access ++ "` "++ show p ++
            " in type `" ++ t ++ "`, only records and eithers can be accessed"

        InvalidGuard t p ->
            "Invalid guard " ++ show p ++
            ", guards most be of type `boolean` but `" ++ show t ++
            "` was provided"

        InvalidRange vname vt vp t1 t2 p ->
            "Invalid range " ++ show p ++", lower bound is `" ++ show t1 ++
            "` and higher bound is `" ++ show t2 ++ "`, but for variable `"
            ++ vname ++ "` was declared as `" ++ show vt ++ "` " ++ show vp

        MemberOfArray member p ->
            "Expected array index instead of member " ++ show p ++
            " member named `" ++ member ++ "`"

        NoMain _ ->
            "No procedure `main` defined in the program"

        LexicalError (Position (line, column)) ->
            "Lexical error at line " ++ show line ++
            ", column " ++ show column

        LexicalError _otherPosition -> undefined

        UnclosedComment ->
            "Comment not closed at end of file"

        OutOfScope name p ->
            "Out of scope variable " ++ show p ++ ": `" ++ name ++
            "` not available in this scope."

        RecursiveType t name p ->
            "Attempted to declare a recursive field named `" ++ name ++
            "` " ++ show p ++" in struct `" ++ t ++ "`"

        UndefinedType nameT p ->
            "Attempted to declare variable of undefined type " ++ show p ++
            ", type `" ++ nameT ++ "`"

        UndefinedProcedure name p ->
            "Call to undeclared procedure " ++ show p ++ " named `" ++
            name ++ "`"

        BadCall name args expArgs p ->
            "Bad call to procedure " ++ show p ++ " named `" ++
            name ++ "`, expected args of types " ++ show (toList expArgs) ++
            ", but instead received " ++ show (toList args)

        BadRead t p ->
            "Bad read " ++ show p ++
            " attempted to read into variable of type `" ++ show t ++
            "`; only booleans, chars, floats and integers can be read"

        BadWrite t p ->
            "Bad write " ++ show p ++
            " attempted to write variable of type `" ++ show t ++
            "`; only booleans, chars, floats, integers and strings " ++
            "can be written"

        BadFinish eret procp retp ->
            "Bad finish " ++ show retp ++
            ", used finish in a procedure declared as answering `" ++
            show eret ++ "` " ++ show procp

        BadAnswer eret aret procp retp ->
            "Bad answer " ++ show retp ++
            ", attempted to answer with an expression of type `" ++
            show aret ++
            "` in a procedure declared " ++ (if eret == EpVoid
                then "without an answer type"
                else "as answering `" ++ show eret ++ "`"
            ) ++ " " ++ show procp

        UnexpectedToken t p ->
            "Unexpected token \n" ++ show t ++ "\n" ++ show p

        BadBinaryExpression op ts ets p ->
            "Bad binary expression " ++ show p ++ ", operator `" ++
            show op ++ "` expected one pair of " ++ show ets ++
            ", but received " ++ show ts

        BadUnaryExpression op t ets p ->
            "Bad unary expression " ++ show p ++ ", operator `" ++
            show op ++ "` expected one of " ++ show ets ++
            ", but received `" ++ show t ++ "`"

        BadForVar n t dp up ->
            "Bad for variable, " ++ show up ++ ", `" ++ n ++
            "`, declared as `" ++ show t ++ "` at " ++ show dp ++
            "; expected `character` or `integer`"

        BadCaseExp t p ->
            "Bad case expression " ++ show p ++
            ", expected expression of type `character` or `integer`, but `" ++
            show t ++ "` was given"

        BadCaseIntElem ep v vp ->
            "Bad element in case expression " ++
            show vp ++ ", element `" ++ show v ++
            "` has type `character` but expression " ++
            show ep ++ " has type `integer`"

        BadCaseCharElem ep v vp ->
            "Bad element in case expression " ++
            show vp ++ ", element `" ++ show v ++
            "` has type `integer` but expression " ++
            show ep ++ " has type `character`"

        BadDeref t p ->
            "Bad dereference " ++ show p ++
            ", attempted to dereference lval of type `" ++ show t ++
            "`, but only Pointer types can be dereferenced"

