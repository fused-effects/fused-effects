{-# LANGUAGE DeriveFunctor, FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, AltC(..)
, Branch(..)
, branch
, runBranch
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
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


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
-- >>> instance (Arbitrary1 m, Arbitrary e) => Arbitrary1 (Branch m e) where liftArbitrary arb = frequency [(1, None <$> arbitrary), (3, Pure <$> arb), (3, Alt <$> liftArbitrary arb <*> liftArbitrary arb)]
-- >>> instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (Branch m e a) where arbitrary = arbitrary1
