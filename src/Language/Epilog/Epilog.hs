{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Epilog.Epilog
    ( Byte
    , Epilog
    , EpilogState (..)
    , ProcSignature (..)
    , Strings
    , Types
    , Procs
    , err
    , get
    , gets
    , initialState
    , modify
    , runEpilog
    -- Lenses
    , symbols, strings, pendProcs, procs, types, expression, position, input
    , prevChar, bytes, scanCode, commentDepth
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Type
import           Language.Epilog.Common
import           Language.Epilog.Error
import           Language.Epilog.Position
import           Language.Epilog.SymbolTable    hiding (empty)
import qualified Language.Epilog.SymbolTable    as ST (empty)
--------------------------------------------------------------------------------
import           Control.Lens                   (makeLenses)
import           Control.Monad.Trans.RWS.Strict (RWS, get, gets, modify, runRWS,
                                                 tell)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map (empty, fromAscList)
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq (empty, singleton)
import           Data.Word                      (Word8)
--------------------------------------------------------------------------------
-- Synonyms ----------------------------
type Strings      = Map String (Seq Position)
type Types        = Map Name (Type, SymbolTable, Position)
type Procs        = Map Name ProcSignature
type Pending      = Map Name (Seq Position)

-- Table Element Types -----------------
data ProcSignature = ProcSignature
    { procName     :: Name
    , procType     :: Type
    , procPosition :: Position
    }

err :: a -> RWS r (Seq a) s ()
err = tell . Seq.singleton

-- State Type --------------------------
type Byte = Word8

-- | The state of the compiler monad. Includes the Lexer and Parser states.
data EpilogState = EpilogState
    { _symbols      :: SymbolTable
    , _strings      :: Strings
    , _pendProcs    :: Pending
    , _procs        :: Procs
    , _types        :: Types
    , _expression   :: Seq Expression

    , _position     :: Position
    , _input        :: String
    , _prevChar     :: Char
    , _bytes        :: [Byte]
    , _scanCode     :: Int
    , _commentDepth :: Int
    }

makeLenses ''EpilogState

languageProcs :: [(Name, ProcSignature)]
languageProcs =
    [ ("toBoolean",   ProcSignature "toBoolean"   boolT  Epilog)
    , ("toCharacter", ProcSignature "toCharacter" charT  Epilog)
    , ("toFloat",     ProcSignature "toFloat"     floatT Epilog)
    , ("toInteger",   ProcSignature "toInteger"   intT   Epilog)
    ] -- Must be ascending

basicTypes :: [(Name, (Type, SymbolTable, Position))]
basicTypes =
    [ ("character", (charT  , ST.empty, Epilog))
    , ("float",     (floatT , ST.empty, Epilog))
    , ("integer",   (intT   , ST.empty, Epilog))
    , ("string",    (stringT, ST.empty, Epilog))
    , ("void",      (voidT  , ST.empty, Epilog))
    ] -- Must be ascending

initialState :: String -> EpilogState
initialState inp = EpilogState
    { _symbols      = ST.empty
    , _strings      = Map.empty
    , _pendProcs    = Map.empty
    , _procs        = Map.fromAscList languageProcs
    , _types        = Map.fromAscList basicTypes
    , _expression   = Seq.empty

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
