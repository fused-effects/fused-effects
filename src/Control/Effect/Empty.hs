{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, abort
) where

import Control.Carrier.Class
import GHC.Generics (Generic1)

data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty

-- | Abort the computation.
--
--   prop> run (runEmpty abort) === Nothing
abort :: (Carrier sig m, Member Empty sig) => m a
abort = send Empty
