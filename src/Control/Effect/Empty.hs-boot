{-# LANGUAGE KindSignatures #-}
module Control.Effect.Empty
( Empty(..)
) where

import Control.Effect.Class

data Empty (m :: * -> *) k = Empty

instance Functor m => Functor (Empty m)
instance HFunctor Empty
instance Effect   Empty
