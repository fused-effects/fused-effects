{-# LANGUAGE DeriveTraversable, FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.NonDet
( NonDet(..)
, Alternative(..)
, runNonDet
, AltC(..)
, runNonDetOnce
, OnceC(..)
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


-- | Run a 'NonDet' effect, returning the first successful result in an 'Alternative' functor.
--
--   Unlike 'runNonDet', this will terminate immediately upon finding a solution.
--
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == [a]
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == Just a
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


-- | The result of a nondeterministic branch of a computation.
data Branch m e a
  = None e
  | Pure a
  | Alt (m a) (m a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Case analysis for 'Branch', taking a value to use for 'Cut', a value to use for 'None', and a function to apply to the contents of 'Pure'.
--
--   prop> branch None Pure Alt a == (a :: Branch e [] a)
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (None a :: Branch [] a) == applyFun f a
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (Pure a :: Branch [] a) == applyFun g a
--   prop> branch (applyFun f) (applyFun g) (applyFun2 h) (Alt a b :: Branch [] a) == applyFun2 h a b
branch :: (e -> a) -> (b -> a) -> (m b -> m b -> a) -> Branch m e b -> a
branch f _ _ (None a)  = f a
branch _ f _ (Pure a)  = f a
branch _ _ f (Alt a b) = f a b
{-# INLINE branch #-}

-- | Interpret a 'Branch' into an underlying 'Alternative' context.
runBranch :: Alternative m => (e -> m a) -> Branch m e a -> m a
runBranch f = branch f pure (<|>)
{-# INLINE runBranch #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
-- >>> instance (Arbitrary1 m, Arbitrary e) => Arbitrary1 (Branch m e) where liftArbitrary arb = frequency [(1, None <$> arbitrary), (3, Pure <$> arb), (3, Alt <$> liftArbitrary arb <*> liftArbitrary arb)]
-- >>> instance (Arbitrary1 m, Arbitrary e, Arbitrary a) => Arbitrary (Branch m e a) where arbitrary = arbitrary1
