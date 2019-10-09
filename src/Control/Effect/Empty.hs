{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, KindSignatures #-}
module Control.Effect.Empty
( -- * Empty effect
  Empty(..)
, empty
, guard
  -- * Properties
, empty_annihilation
  -- * Re-exports
, Has
) where

import {-# SOURCE #-} Control.Carrier
import GHC.Generics (Generic1)

-- | An effect modelling nondeterminism without choice.
--
--   This can be seen as similar to 'Control.Effect.Fail.Fail', but without an error message.
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty

-- | Abort the computation.
--
-- 'empty' annihilates '>>=':
--
-- @
-- 'empty' '>>=' k = 'empty'
-- @
empty :: Has Empty sig m => m a
empty = send Empty

-- | Conditional failure, returning only if the condition is 'True'.
guard :: Has Empty sig m => Bool -> m ()
guard True  = pure ()
guard False = empty


-- Properties

empty_annihilation :: Has Empty sig m => (m b -> m b -> prop) -> (a -> m b) -> prop
empty_annihilation (===) k = (empty >>= k) === empty
