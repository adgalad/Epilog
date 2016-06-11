{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Epilog
    ( Byte
    , Epilog
    , EpilogState (..)
    , ProcSignature (..)
    , Strings
    , Types
    , err
    , get
    , gets
    , initialState
    , modify
    , runEpilog
    -- Lenses
    , symbols, strings, pendProcs, types, expression, position, input
    , prevChar, bytes, scanCode, commentDepth, current, curfields
    , curkind, forVars, caseTypes, offset
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.Type
import           Language.Epilog.Common
import           Language.Epilog.Error
import           Language.Epilog.At
import           Language.Epilog.SymbolTable    hiding (empty)
--------------------------------------------------------------------------------
import           Control.Lens                   (makeLenses)
import           Control.Monad.Trans.RWS.Strict (RWS, get, gets, modify, runRWS,
                                                 tell)
import           Data.Map.Strict                (Map)
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq (singleton)
import           Data.Word                      (Word8)
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Strings      = Map String (Seq Position)
type Types        = Map Name   (Type, Position)
type Pending      = Map Name   (Seq Position)

-- Table Element Types -----------------
data ProcSignature = ProcSignature
    { procName     :: Name
    , procType     :: Type
    , procPosition :: Position
    }

err :: a -> RWS r (Seq a) s ()
err = tell . Seq.singleton

-- State Types -------------------------
type Byte       = Word8

-- | The state of the compiler monad. Includes the Lexer and Parser states.
data EpilogState = EpilogState
    { _symbols      :: SymbolTable
    , _strings      :: Strings
    , _pendProcs    :: Pending
    , _types        :: Types
    , _expression   :: Seq Expression
    , _current      :: Maybe (At Name)
    , _curfields    :: Seq (At Name, Type)
    , _curkind      :: Maybe StructKind
    , _forVars      :: [(At Name, Type)]
    , _caseTypes    :: [At Type]
    , _offset       :: [Int]

    , _position     :: Position
    , _input        :: String
    , _prevChar     :: Char
    , _bytes        :: [Byte]
    , _scanCode     :: Int
    , _commentDepth :: Int
    }

makeLenses ''EpilogState

predefinedProcs :: [Entry]
predefinedProcs =
    [ entry "toBoolean"
        ([OneOf [        charT, floatT, intT ]] :-> boolT ) Epilog 0 4
    , entry "toCharacter"
        ([OneOf [ boolT,        floatT, intT ]] :-> charT ) Epilog 0 4
    , entry "toFloat"
        ([OneOf [ boolT, charT,         intT ]] :-> floatT) Epilog 0 4
    , entry "toInteger"
        ([OneOf [ boolT, charT, floatT       ]] :-> intT  ) Epilog 0 4
    , entry "length"
        ([Array 0 0 Any 4] :-> intT  ) Epilog 0 4
    ]

basicTypes :: Map Name (Type, Position)
basicTypes =
    [ ("boolean"  , ( boolT  , Epilog ))
    , ("character", ( charT  , Epilog ))
    , ("float"    , ( floatT , Epilog ))
    , ("integer"  , ( intT   , Epilog ))
    , ("string"   , ( stringT, Epilog ))
    , ("void"     , ( voidT  , Epilog ))
    ]

initialST :: SymbolTable
initialST = foldr aux (emptyP Epilog) predefinedProcs
    where
        aux e @ Entry { eName } =
            insertSymbol eName e

initialState :: String -> EpilogState
initialState inp = EpilogState
    { _symbols      = initialST
    , _strings      = []
    , _pendProcs    = []
    , _types        = basicTypes
    , _expression   = []
    , _current      = Nothing
    , _curfields    = []
    , _curkind      = Nothing
    , _forVars      = []
    , _caseTypes    = []
    , _offset       = [0]

    , _position     = Position (1, 1)
    , _input        = inp
    , _prevChar     = '\n'
    , _bytes        = []
    , _scanCode     = 0
    , _commentDepth = 0
    }


-- The Monad ---------------------------
type Epilog = RWS () Errors EpilogState

runEpilog :: RWS r w s a -> r -> s -> (a, s, w)
runEpilog = runRWS
