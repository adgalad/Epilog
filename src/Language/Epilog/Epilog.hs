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
    , Procs
    , err
    , get
    , gets
    , initialState
    , modify
    , runEpilog
    -- Lenses
    , symbols, strings, pendProcs, types, expression, position, input
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
type Types        = Map Name (Type, Position)
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

predefinedProcs :: [Entry]
predefinedProcs =
    [ entry "toBoolean"   ([Any] :-> boolT ) Epilog
    , entry "toCharacter" ([Any] :-> charT ) Epilog
    , entry "toFloat"     ([Any] :-> floatT) Epilog
    , entry "toInteger"   ([Any] :-> intT  ) Epilog
    ] -- Must be ascending

basicTypes :: [(Name, (Type, Position))]
basicTypes =
    [ ("boolean"  , ( boolT  , Epilog ))
    , ("character", ( charT  , Epilog ))
    , ("float"    , ( intT   , Epilog ))
    , ("integer"  , ( floatT , Epilog ))
    , ("string"   , ( stringT, Epilog ))
    ] -- Must be ascending

initialST :: SymbolTable
initialST = foldr aux ST.empty predefinedProcs
    where
        aux e @ Entry { eName } st =
            insertSymbol eName e st

initialState :: String -> EpilogState
initialState inp = EpilogState
    { _symbols      = initialST
    , _strings      = Map.empty
    , _pendProcs    = Map.empty
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
