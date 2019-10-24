{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
module Control.Effect.Empty
( Empty(..)
) where

import Control.Effect.Class

data Empty (m :: * -> *) k = Empty

instance Functor f => Effect f Empty
