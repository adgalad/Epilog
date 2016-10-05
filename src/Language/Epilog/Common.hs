module Language.Epilog.Common
  ( Name
  , Semigroup (..)
  , Map
  , Seq
  , Int32
  , Word8
  , toList
  , fromJust
  , (|>=)
  , (|>~)
  , (<|=)
  , (<|~)
  , liftIO
  , internal
  ) where

import           Data.Foldable   (toList, foldrM, foldlM)
import           Data.Map.Strict (Map)
import           Data.Int        (Int32)
import           Data.Word       (Word8)
import           Data.Maybe      (fromJust)
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup  (Semigroup (..))
import           Data.Sequence   (Seq)
import           Control.Lens    (ASetter, (%=), (|>), (%~), Snoc, Cons, (<|))

type Name = String

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
