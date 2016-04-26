module Language.Epilog.Classes
    ( NiceShow
    , niceShow
    ) where

class NiceShow a where
    niceShow :: a -> String
