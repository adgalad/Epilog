{-# LANGUAGE LambdaCase #-}

module Language.Epilog.Error
    ( EpilogError (..)
    , Errors
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Type hiding (name)
import           Language.Epilog.Common
import           Language.Epilog.Position
import           Language.Epilog.Token
--------------------------------------------------------------------------------
import           Data.Function            (on)
import           Data.Sequence            (Seq)
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
    | UndefinedType
        { utTName :: Name
        , utP     :: Position
        }
    | UndefinedProcedure
        { upName :: Name
        , upP    :: Position
        }
    | InvalidMember
        { imName :: Name
        , imP    :: Position
        }
    | MemberOfArray
        { moaName :: Name
        , moaP    :: Position
        }
    | InvalidIndex
        { iiP :: Position }
    | NoMain
        { nmP :: Position }
    | LexicalError
        { leP :: Position }
    | UnclosedComment
    | Underflow
        { uMsg :: String
        , uP   :: Position
        }
    | Overflow
        { oMsg :: String
        , oP   :: Position
        }
    | UnclosedStringLit
        { uslVal :: String
        , uslP   :: Position
        }
    | UnexpectedToken
        { utT :: Token
        , utP :: Position
        }
    deriving (Eq)

instance P EpilogError where
    pos = \case
        DuplicateDefinition      _ _ p -> p
        OutOfScope                 _ p -> p
        DuplicateDeclaration _ _ _ _ p -> p
        UndefinedType              _ p -> p
        UndefinedProcedure         _ p -> p
        InvalidMember              _ p -> p
        MemberOfArray              _ p -> p
        InvalidIndex                 p -> p
        NoMain                       p -> p
        LexicalError                 p -> p
        UnclosedComment                -> Code
        Underflow                  _ p -> p
        Overflow                   _ p -> p
        UnclosedStringLit          _ p -> p
        UnexpectedToken            _ p -> p

instance Ord EpilogError where
    compare = compare `on` pos

instance Show EpilogError where
    show = \case
        DuplicateDefinition name fstP sndP ->
            "Duplicate definition " ++ showP sndP ++ ": `" ++ name ++
            "` already defined " ++ showP fstP

        OutOfScope name p ->
            "Out of scope variable " ++ showP p ++ ": `" ++ name ++
            "` not available in this scope."

        DuplicateDeclaration name fstT fstP sndT sndP ->
            "Duplicate declaration " ++ showP sndP ++ ", variable `" ++
            name ++ "` already defined as `" ++ show fstT ++ "` " ++
            showP fstP ++ " cannot be redeclared as `" ++ show sndT ++ "`"

        UndefinedType nameT p ->
            "Attempted to declare variable of undefined type " ++ showP p ++
            ", type `" ++ nameT ++ "`"

        UndefinedProcedure  name p ->
            "Call to undeclared procedure " ++ showP p ++ " named `" ++
            name ++ "`"

        InvalidMember member p ->
            "Invalid member requested " ++ showP p ++ " named `" ++
            member ++ "`"

        MemberOfArray member p ->
            "Expected array index instead of member " ++ showP p ++
            " member named `" ++ member ++ "`"

        InvalidIndex p ->
            "Index of non-array variable " ++ showP p

        NoMain _ ->
            "No procedure `main` defined in the program"

        LexicalError (Position (line, column)) ->
            "Lexical error at line " ++ show line ++
            ", column " ++ show column

        LexicalError _ -> undefined

        UnclosedComment ->
            "Comment not closed at end of file"

        Underflow msg p ->
            "Underflow \n" ++ msg ++ "\n" ++ show p

        Overflow msg p ->
            "Overflow \n" ++ msg ++ "\n" ++ show p

        UnclosedStringLit val p ->
            "Unclosed String Literal \n" ++ val ++ "\n" ++ show p

        UnexpectedToken t p ->
            "Unexpected token \n" ++ show t ++ "\n" ++ show p
