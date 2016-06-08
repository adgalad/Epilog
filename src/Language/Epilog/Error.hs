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
    | InvalidAssign
        { iaFstT :: Type
        , iaSndT :: Type
        , iaP    :: Position
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
    deriving (Eq)

instance P EpilogError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        DuplicateField _ _ _ _ _ _ _ p -> p
        InvalidAssign            _ _ p -> p
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
            "Duplicate definition " ++ showP sndP ++ ", `" ++ name ++
            "` already defined " ++ showP fstP

        InvalidAssign t1 t2 p->
            "Invalid assignment " ++ showP p ++
            ", attempted to assign an expression of type `" ++ show t2 ++
            "` to an lval of type `" ++ show t1 ++ "` "

        InvalidArray p ->
            "Index of non-array variable " ++ showP p

        InvalidSubindex t p ->
            "Invalid index " ++ showP p ++
            ", attempted to use an expression of type `" ++ show t ++
            "` as a subindex"

        InvalidField field t p ->
            "No field named `" ++ field ++ "` "++ showP p ++
            " in record `" ++ t ++ "`"

        InvalidMember member t p ->
            "No member named `" ++ member ++ "` "++ showP p ++
            " in either `" ++ t ++ "`"

        InvalidAccess access t p ->
            "Invalid access `" ++ access ++ "` "++ showP p ++
            " in type `" ++ t ++ "`, only records and eithers can be accessed"

        InvalidGuard t p ->
            "Invalid guard " ++ showP p ++
            ", guards most be of type `boolean` but `" ++ show t ++
            "` was provided"

        InvalidRange vname vt vp t1 t2 p ->
            "Invalid range " ++ showP p ++", lower bound is `" ++ show t1 ++
            "` and higher bound is `" ++ show t2 ++ "`, but for variable `"
            ++ vname ++ "` was declared as `" ++ show vt ++ "` at " ++ showP vp

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

        BadRead t p ->
            "Bad read " ++ showP p ++
            " attempted to read to variable of type `" ++ show t ++ "`"

        BadWrite t p ->
            "Bad write " ++ showP p ++
            " attempted to write to variable of type `" ++ show t ++ "`"

        BadFinish eret      procp retp ->
            "Bad finish " ++ showP retp ++
            ", used finish in a procedure declared as answering `" ++
            show eret ++ "` at " ++ showP procp

        BadAnswer eret aret procp retp ->
            "Bad answer " ++ showP retp ++
            ", attempted to answer with an expression of type `" ++
            show aret ++
            "` in a procedure declared " ++ (if eret == voidT
                then "without an answer type"
                else "as answering `" ++ show eret ++ "`"
            ) ++ " at " ++ showP procp

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

        BadForVar n t dp up ->
            "Bad for variable, " ++ showP up ++ ", `" ++ n ++
            "`, declared as `" ++ show t ++ "` at " ++ showP dp ++
            "; expected `character` or `integer`"

        BadCaseExp t p ->
            "Bad case expression " ++ showP p ++
            ", expected expression of type `character` or `integer`, but `" ++
            show t ++ "` was given"

        BadCaseIntElem ep v vp ->
            "Bad element in case expression " ++
            showP vp ++ ", element `" ++ show v ++
            "` has type `character` but expression at " ++
            showP ep ++ " has type `integer`"

        BadCaseCharElem ep v vp ->
            "Bad element in case expression " ++
            showP vp ++ ", element `" ++ show v ++
            "` has type `integer` but expression at " ++
            showP ep ++ " has type `character`"
