{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, AltH(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum
import Control.Monad (join)

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: (Alternative f, Monad f, Traversable f, Effectful sig m) => Eff (AltH f m) a -> m (f a)
runNonDet = runAltH . interpret

newtype AltH f m a = AltH { runAltH :: m (f a) }

instance (Alternative f, Monad f, Traversable f, Effectful sig m) => Carrier (NonDet :+: sig) (AltH f m) where
  gen a = AltH (pure (pure a))
  alg = algND \/ (AltH . alg . handle (pure ()) (fmap join . traverse runAltH))
    where algND Empty      = AltH (pure empty)
          algND (Choose k) = AltH (liftA2 (<|>) (runAltH (k True)) (runAltH (k False)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
