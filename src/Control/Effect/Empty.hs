{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, abort
) where

import Control.Carrier.Class
import GHC.Generics (Generic1)

-- | An effect modelling nondeterminism without choice.
--
--   This can be seen as similar to 'Control.Effect.Fail', but without an error message.
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty

-- | Abort the computation.
--
--   prop> run (runEmpty abort) === Nothing
abort :: (Carrier sig m, Member Empty sig) => m a
abort = send Empty
