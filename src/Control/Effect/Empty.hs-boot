{-# LANGUAGE KindSignatures #-}
module Control.Effect.Empty
( Empty(..)
) where

import Control.Effect.Class

data Empty (m :: * -> *) k = Empty

instance HFunctor Empty
