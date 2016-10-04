{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Epilog.Epilog
    ( Byte
    , Epilog
    , EpilogConfig (..)
    , EpilogState (..)
    , Strings
    , Types
    , err
    , get
    , gets
    , initialState
    , mipsConfig
    , modify
    , runEpilog
    , runEpilog'
    -- State Lenses
    , symbols, strings, pendProcs, types, position, input
    , prevChar, bytes, scanCode, commentDepth, current, curfields
    , curkind, forVars, caseTypes, offset, curProcType, structSize
    , structAlign, parameters, procedures, parseOK, entryKind
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Instruction
import           Language.Epilog.AST.Procedure
import           Language.Epilog.At
import           Language.Epilog.Common
import           Language.Epilog.SymbolTable     hiding (empty)
import           Language.Epilog.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (makeLenses, (.=))
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.RWS.Strict  (RWST, get, gets, modify,
                                                  runRWST)
import           System.IO                       (hPrint, stderr)
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Strings = Map String Int32
type Types   = Map Name   (Type, Position)
type Pending = Map Name   (Seq Position)
type Byte    = Word8

-- | The configuration of the compiler monad.
data EpilogConfig = EpilogConfig
  { basicTypes      :: Map Name (Type, Position)
  , predefinedProcs :: [Entry]
  , pointerSize     :: Int
  , pointerAlign    :: Int }

makeLenses ''EpilogConfig


mipsConfig :: EpilogConfig
mipsConfig = EpilogConfig
  { basicTypes      = mipsTypes
  , predefinedProcs = mipsProcs
  , pointerSize     = mipsPointerSize
  , pointerAlign    = mipsPointerAlign }
  where
    mipsTypes =
      [ ("boolean"  , ( Basic EpBoolean   1 4, Epilog ))
      , ("character", ( Basic EpCharacter 1 4, Epilog ))
      , ("float"    , ( Basic EpFloat     4 4, Epilog ))
      , ("integer"  , ( Basic EpInteger   4 4, Epilog ))
      , ("string"   , ( EpStr             0 1, Epilog ))
      , ("void"     , ( EpVoid               , Epilog )) ]
    mipsProcs = []
        -- [ func "toBoolean"
        --     ([OneOf [        charT, floatT, intT ]] :-> boolT ) Epilog 0
        -- , func "toCharacter"
        --     ([OneOf [ boolT,        floatT, intT ]] :-> charT ) Epilog 0
        -- , func "toFloat"
        --     ([OneOf [ boolT, charT,         intT ]] :-> floatT) Epilog 0
        -- , func "toInteger"
        --     ([OneOf [ boolT, charT, floatT       ]] :-> intT  ) Epilog 0
        -- , func "length"
        --     ([Array 0 0 Any 4 0]                    :-> intT  ) Epilog 0
        -- , func "make"
        --     ([Pointer Any 0 0]                      :-> EpVoid) Epilog 0
        -- , func "ekam"
        --     ([Pointer Any 0 0]                      :-> EpVoid) Epilog 0
        -- ]
    mipsPointerSize  = 4
    mipsPointerAlign = 4

-- | The state of the compiler monad. Includes the Lexer and Parser states.
data EpilogState = EpilogState
  { _symbols      :: SymbolTable
  , _entryKind    :: EntryKind
  , _parseOK      :: Bool
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
  , _curProcType  :: Type

  , _procedures   :: Map String Procedure
  , _parameters   :: Params

  , _position     :: Position
  , _input        :: String
  , _prevChar     :: Char
  , _bytes        :: [Byte]
  , _scanCode     :: Int
  , _commentDepth :: Int }

makeLenses ''EpilogState


initialState :: String -> EpilogState
initialState inp = EpilogState
  { _symbols      = emptyP Epilog
  , _entryKind    = Global
  , _parseOK      = True
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
  , _curProcType  = None

  , _procedures   = []
  , _parameters   = []

  , _position     = Position 1 1
  , _input        = inp
  , _prevChar     = '\n'
  , _bytes        = []
  , _scanCode     = 0
  , _commentDepth = 0 }


-- The Monad ---------------------------
type Epilog = RWST EpilogConfig () EpilogState IO

runEpilog' :: Epilog a -> EpilogConfig -> EpilogState -> IO (a, EpilogState, ())
runEpilog' = runRWST

runEpilog :: Epilog a
          -> String
          -> IO (a, EpilogState, ())
runEpilog x inp = runEpilog' x mipsConfig (initialState inp)

err :: Show a => a -> Epilog ()
err msg = do
  parseOK .= False
  liftIO . hPrint stderr $ msg
