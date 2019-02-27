{-# LANGUAGE DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
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
import Control.Effect.Cull
import Control.Effect.NonDet.Internal
import Control.Effect.Sum
import Control.Monad (MonadPlus(..), join)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Monoid as Monoid (Alt(..))
import Prelude hiding (fail)

-- | Run a 'NonDet' effect, collecting all branches’ results into an 'Alternative' functor.
--
--   Using '[]' as the 'Alternative' functor will produce all results, while 'Maybe' will return only the first. However, unlike 'runNonDetOnce', this will still enumerate the entire search space before returning, meaning that it will diverge for infinite search spaces, even when using 'Maybe'.
--
--   prop> run (runNonDet (pure a)) == [a]
--   prop> run (runNonDet (pure a)) == Just a
runNonDet :: AltC f m a -> m (f a)
runNonDet = runAltC

newtype AltC f m a = AltC { runAltC :: m (f a) }
  deriving (Functor)

instance (Applicative f, Applicative m) => Applicative (AltC f m) where
  pure = AltC . pure . pure
  AltC f <*> AltC a = AltC (liftA2 (<*>) f a)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Alternative (AltC f m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Monad (AltC f m) where
  AltC a >>= f = AltC (a >>= runNonDet . getAlt . foldMap (Monoid.Alt . f))

instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadFail m, Traversable f) => MonadFail (AltC f m) where
  fail s = AltC (fail s)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadIO m, Traversable f) => MonadIO (AltC f m) where
  liftIO io = AltC (pure <$> liftIO io)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Carrier (NonDet :+: sig) (AltC f m) where
  eff = AltC . handleSum (eff . handle (pure ()) (fmap join . traverse runNonDet)) (\case
    Empty    -> pure empty
    Choose k -> liftA2 (<|>) (runNonDet (k True)) (runNonDet (k False)))


-- | Run a 'NonDet' effect, returning the first successful result in an 'Alternative' functor.
--
--   Unlike 'runNonDet', this will terminate immediately upon finding a solution.
--
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == [a]
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == Just a
runNonDetOnce :: (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => OnceC f m a -> m (f a)
runNonDetOnce = runNonDet . runCull . cull . runOnceC

newtype OnceC f m a = OnceC { runOnceC :: CullC (AltC f m) a }
  deriving (Functor)

deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Applicative (OnceC f m)
deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Alternative (OnceC f m)
deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Monad (OnceC f m)
deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadFail m, Traversable f) => MonadFail (OnceC f m)
deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, MonadIO m, Traversable f) => MonadIO (OnceC f m)
deriving instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => MonadPlus (OnceC f m)

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Traversable f) => Carrier (NonDet :+: sig) (OnceC f m) where
  eff = OnceC . handleSum (eff . R . R . R . handleCoercible) (\case
    Empty    -> empty
    Choose k -> runOnceC (k True) <|> runOnceC (k False))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
