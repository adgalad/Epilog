{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Epilog.Epilog
    ( Byte
    , Epilog
    , EpilogConfig (..)
    , EpilogState (..)
    , ProcSignature (..)
    , Strings
    , Types
    , err
    , get
    , gets
    , initialState
    , mipsConfig
    , modify
    , runEpilog
    -- State Lenses
    , symbols, strings, pendProcs, types, expression, position, input
    , prevChar, bytes, scanCode, commentDepth, current, curfields
    , curkind, forVars, caseTypes, offset, instructions, guards
    , curProcType, sets, ranges, caseSet, lvals, ast, structSize
    , structAlign
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Type
import           Language.Epilog.Common
import           Language.Epilog.At
import           Language.Epilog.SymbolTable    hiding (empty)
--------------------------------------------------------------------------------
import           Control.Lens                   (makeLenses)
import           Control.Monad.Trans.RWS.Strict (RWST, get, gets, modify,
                                                 runRWST)
import           Data.Map.Strict                (Map)
import           Data.Sequence                  (Seq)
import           Data.Word                      (Word8)
import           System.IO                      (hPrint, stderr)
import           Control.Monad.IO.Class         (liftIO)
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Strings = Map String (Seq Position)
type Types   = Map Name   (Type, Position)
type Pending = Map Name   (Seq Position)
type Byte    = Word8

-- | The configuration of the compiler monad.
data EpilogConfig = EpilogConfig
    {
    --padding         :: Int  -> Int,
      basicTypes      :: Map Name (Type, Position)
    , predefinedProcs :: [Entry]
    , pointerSize     :: Int
    , pointerAlign    :: Int
    }

makeLenses ''EpilogConfig


mipsConfig :: EpilogConfig
mipsConfig = EpilogConfig
    { basicTypes      = mipsTypes
    , predefinedProcs = mipsProcs
    , pointerSize     = mipsPointerSize
    , pointerAlign    = mipsPointerAlign
    }
    where
        mipsTypes =
            [ ("boolean"  , ( Basic EpBoolean   1 4, Epilog ))
            , ("character", ( Basic EpCharacter 1 4, Epilog ))
            , ("float"    , ( Basic EpFloat     4 4, Epilog ))
            , ("integer"  , ( Basic EpInteger   4 4, Epilog ))
            , ("string"   , ( EpStr             0 1, Epilog ))
            , ("void"     , ( EpVoid               , Epilog ))
            ]
        mipsProcs =
            [ entry "toBoolean"
                ([OneOf [        charT, floatT, intT ]] :-> boolT ) Epilog 0
            , entry "toCharacter"
                ([OneOf [ boolT,        floatT, intT ]] :-> charT ) Epilog 0
            , entry "toFloat"
                ([OneOf [ boolT, charT,         intT ]] :-> floatT) Epilog 0
            , entry "toInteger"
                ([OneOf [ boolT, charT, floatT       ]] :-> intT  ) Epilog 0
            , entry "length"
                ([Array 0 0 Any 4 0]                    :-> intT  ) Epilog 0
            , entry "make"
                ([Pointer Any 0 0]                      :-> EpVoid) Epilog 0
            , entry "ekam"
                ([Pointer Any 0 0]                      :-> EpVoid) Epilog 0
            ]
        mipsPointerSize  = 4
        mipsPointerAlign = 4


---- Table Element Types ---------------
data ProcSignature = ProcSignature
    { procName     :: Name
    , procType     :: Type
    , procPosition :: Position
    }


err :: Show a => a -> RWST r () s IO ()
err = liftIO . hPrint stderr


-- | The state of the compiler monad. Includes the Lexer and Parser states.
data EpilogState = EpilogState
    { _symbols      :: SymbolTable
    , _strings      :: Strings
    , _pendProcs    :: Pending
    , _types        :: Types
    , _current      :: Maybe (At Name)
    , _curfields    :: Seq (At Name, Type, Int)
    , _curkind      :: Maybe StructKind
    , _structSize   :: Int
    , _structAlign  :: Int
    , _forVars      :: [(At Name, Type)]
    , _caseTypes    :: [At Type]
    , _offset       :: [Int]
    -- AST
    , _caseSet      :: Exps
    , _curProcType  :: Type
    ------ , _currentEntry :: Entry
    , _instructions :: [Insts]
    , _lvals        :: [Expression]
    , _expression   :: Exps
    , _guards       :: Guards
    , _sets         :: Sets
    , _ranges       :: Ranges
    , _ast          :: Insts

    , _position     :: Position
    , _input        :: String
    , _prevChar     :: Char
    , _bytes        :: [Byte]
    , _scanCode     :: Int
    , _commentDepth :: Int
    }

makeLenses ''EpilogState


initialState :: String -> EpilogState
initialState inp = EpilogState
    { _symbols      = emptyP Epilog
    , _strings      = []
    , _pendProcs    = []
    , _types        = []
    , _current      = Nothing
    , _curfields    = []
    , _curkind      = Nothing
    , _structSize   = 0
    , _structAlign  = 0
    , _forVars      = []
    , _caseTypes    = []
    , _offset       = [0]
    -- AST
    , _caseSet      = []
    , _curProcType  = None
    , _instructions = [[]]
    , _lvals        = []
    , _expression   = []
    , _guards       = []
    , _sets         = []
    , _ranges       = []
    , _ast          = []

    , _position     = Position (1, 1)
    , _input        = inp
    , _prevChar     = '\n'
    , _bytes        = []
    , _scanCode     = 0
    , _commentDepth = 0
    }


-- The Monad ---------------------------
type Epilog = RWST EpilogConfig () EpilogState IO

runEpilog :: RWST r w s IO a -> r -> s -> IO (a, s, w)
runEpilog = runRWST
