{-# LANGUAGE MagicHash #-}

module Language.Epilog.Unsafe
  ( encodeIEEEFloat
  , decodeIEEEFloat
  ) where

import           GHC.Prim  (unsafeCoerce#)
import           GHC.Types (Float (F#))
import           GHC.Word  (Word32 (W32#))

encodeIEEEFloat :: Float -> Word32
encodeIEEEFloat (F# x) = W32# (unsafeCoerce# x)

decodeIEEEFloat :: Word32 -> Float
decodeIEEEFloat (W32# x) = F# (unsafeCoerce# x)
