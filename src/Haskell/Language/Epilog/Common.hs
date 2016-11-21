module Language.Epilog.Common
  ( Name
  , Offset
  , Size
  , Semigroup (..)
  , Map
  , Seq
  , Int32
  , Word8
  , Word32
  , toList
  , fromJust
  , (|>=)
  , (|>~)
  , (<|=)
  , (<|~)
  , liftIO
  , foldM
  , forM_
  , unless
  , when
  , internal
  ) where

import           Control.Lens              (ASetter, Cons, Snoc, (%=), (%~),
                                            (<|), (|>))
import           Control.Monad             (foldM, forM_, unless, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State.Class (MonadState)
import           Data.Foldable             (toList)
import           Data.Int                  (Int32)
import           Data.Map.Strict           (Map)
import           Data.Maybe                (fromJust)
import           Data.Semigroup            (Semigroup (..))
import           Data.Sequence             (Seq)
import           Data.Word                 (Word32, Word8)

type Name = String
type Offset = Int32
type Size = Word32

infix 4 |>=
(|>=) :: (Snoc b b a a, MonadState s m)
      => ASetter s s b b -> a -> m ()
xs |>= x = xs %= (|> x)

infixr 4 |>~
(|>~) :: Snoc b b a a
      => ASetter s t b b -> a -> s -> t
xs |>~ x = xs %~ (|> x)

infix 4 <|=
(<|=) :: (Cons b b a a, MonadState s m)
      => ASetter s s b b -> a -> m ()
xs <|= x = xs %= (x <|)

infixr 4 <|~
(<|~) :: Cons b b a a
      => ASetter s t b b -> a -> s -> t
xs <|~ x = xs %~ (x <|)

internal :: String -> a
internal = error . ("internal error: " <>)
