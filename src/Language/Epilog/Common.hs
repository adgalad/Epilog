module Language.Epilog.Common
  ( Name
  , Semigroup (..)
  , Map
  , Seq
  , toList
  , fromJust
  , (|>=)
  , (|>~)
  , (<|=)
  , (<|~)
  , internal
  ) where

import           Data.Foldable   (toList)
import           Data.Map.Strict (Map)
import           Data.Maybe      (fromJust)
import           Control.Monad.State.Class (MonadState)
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
