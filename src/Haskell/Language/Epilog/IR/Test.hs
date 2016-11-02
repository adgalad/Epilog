{-# LANGUAGE PostfixOperators #-}

module Test (test) where

import           Language.Epilog.IR.Monad
import           Language.Epilog.IR.TAC
import           Prelude                  hiding (Ordering (..))

test :: IRMonad ()
test = do
  firstOne <- newLabel

  a <- newRegister "a"

  (firstOne #)
  addTAC $ Comment "my first IR Block :)"
  addTAC $ a := (Id . C . BC $ True)

  t <- newTemp

  l1 <- newLabel
  l2 <- newLabel
  l3 <- newLabel
  addTAC $ t := B AddF a a
  terminate CondBr
    { rel = LT
    , op1 = t
    , op2 = a
    , trueDest  = l1
    , falseDest = l2 }

  (l1 #)
  addTAC $ Comment "my second IR Block :D"
  c <- newRegister "c"
  d <- newRegister "d"

  addTAC $ a :=# (c, d)
  terminate Br
    { dest = l3 }

  (l2 #)
  addTAC $ Comment "my third IR Block... :<"
  k <- newRegister "k"

  addTAC $ k :*= a

  terminate Br
    { dest = l3 }

  (l3 #)
  addTAC $ Comment "It's the final block :<<<"

  terminate Exit
