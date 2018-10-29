{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, AltC(..)
, runNonDetOnce
, OnceC(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: (Alternative f, Monad f, Traversable f, Carrier sig m, Effect sig, Applicative m) => Eff (AltC f m) a -> m (f a)
runNonDet = runAltC . interpret

newtype AltC f m a = AltC { runAltC :: m (f a) }

instance (Alternative f, Monad f, Traversable f, Carrier sig m, Effect sig, Applicative m) => Carrier (NonDet :+: sig) (AltC f m) where
  ret a = AltC (ret (pure a))
  eff = AltC . handleSum (eff . handleTraversable runAltC) (\case
    Empty    -> ret empty
    Choose k -> liftA2 (<|>) (runAltC (k True)) (runAltC (k False)))


runNonDetOnce :: (Alternative f, Monad f, Traversable f, Carrier sig m, Effect sig, Monad m) => Eff (OnceC f m) a -> m (f a)
runNonDetOnce = runOnceC . interpret

newtype OnceC f m a = OnceC { runOnceC :: m (f a) }

instance (Alternative f, Monad f, Traversable f, Carrier sig m, Effect sig, Monad m) => Carrier (NonDet :+: sig) (OnceC f m) where
  ret a = OnceC (ret (pure a))
  eff = OnceC . handleSum (eff . handleTraversable runOnceC) (\case
    Empty    -> ret empty
    Choose k -> do
      l <- runOnceC (k True)
      if null l then
        runOnceC (k False)
      else
        pure l)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
