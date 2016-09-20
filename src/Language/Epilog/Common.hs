module Language.Epilog.Common
    ( Name
    , Semigroup (..)
    , Map
    , Seq
    ) where

import           Data.Map.Strict (Map)
import           Data.Semigroup  (Semigroup (..))
import           Data.Sequence   (Seq)

type Name = String
