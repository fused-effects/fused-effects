{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Reader
( -- * Reader effect
  Reader(..)
, ask
, asks
, local
) where

import {-# SOURCE #-} Control.Carrier

data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

deriving instance Functor m => Functor (Reader r m)

instance HFunctor (Reader r) where
  hmap f (Ask k)       = Ask           (f . k)
  hmap f (Local g m k) = Local g (f m) (f . k)

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

-- | Retrieve the environment value.
--
--   prop> run (runReader a ask) === a
ask :: Has (Reader r) sig m => m r
ask = send (Ask pure)

-- | Project a function out of the current environment value.
--
--   prop> snd (run (runReader a (asks (applyFun f)))) === applyFun f a
asks :: Has (Reader r) sig m => (r -> a) -> m a
asks f = send (Ask (pure . f))

-- | Run a computation with an environment value locally modified by the passed function.
--
--   prop> run (runReader a (local (applyFun f) ask)) === applyFun f a
--   prop> run (runReader a ((,,) <$> ask <*> local (applyFun f) ask <*> ask)) === (a, applyFun f a, a)
local :: Has (Reader r) sig m => (r -> r) -> m a -> m a
local f m = send (Local f m pure)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
-- >>> import Control.Carrier.Reader
