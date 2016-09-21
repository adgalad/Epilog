module Language.Epilog.Common
    ( Name
    , Semigroup (..)
    , Map
    , Seq
    , toList
    , fromJust
    ) where

import           Data.Foldable   (toList)
import           Data.Map.Strict (Map)
import           Data.Maybe      (fromJust)
import           Data.Semigroup  (Semigroup (..))
import           Data.Sequence   (Seq)

type Name = String
