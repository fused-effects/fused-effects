{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cull
( -- * Cull effect
  Cull(..)
, cull
  -- * Cull carriers
, runCull
, CullC(..)
, runNonDetOnce
, OnceC(..)
-- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.Reader
import Control.Effect.Choose
import Control.Effect.Empty
import Control.Effect.NonDet
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Prelude hiding (fail)

-- | 'Cull' effects are used with 'NonDet' to provide control over branching.
data Cull m k
  = forall a . Cull (m a) (a -> m k)

deriving instance Functor m => Functor (Cull m)

instance HFunctor Cull where
  hmap f (Cull m k) = Cull (f m) (f . k)
  {-# INLINE hmap #-}

instance Effect Cull where
  handle state handler (Cull m k) = Cull (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Cull nondeterminism in the argument, returning at most one result.
--
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b)))) === [a]
--   prop> run (runNonDet (runCull (cull (empty  <|> pure a)))) === [a]
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b) <|> pure c))) === [a, c]
--   prop> run (runNonDet (runCull (cull (asum (map pure (repeat a)))))) === [a]
cull :: (Carrier sig m, Member Cull sig) => m a -> m a
cull m = send (Cull m pure)


-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) === [a, b]
runCull :: Alternative m => CullC m a -> m a
runCull (CullC m) = runNonDetC (runReader False m) (<|>) pure empty

newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}
  l <|> r = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDetC (runReader cull (runCullC l)) fork leaf (runNonDetC (runReader cull (runCullC r)) fork leaf nil)
    else
      runReader cull (runCullC l) <|> runReader cull (runCullC r)
  {-# INLINE (<|>) #-}

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance (Carrier sig m, Effect sig) => Carrier (Cull :+: Empty :+: Choose :+: sig) (CullC m) where
  eff (L (Cull m k))         = CullC (local (const True) (runCullC m)) >>= k
  eff (R (L Empty))          = empty
  eff (R (R (L (Choose k)))) = k True <|> k False
  eff (R (R (R other)))      = CullC (eff (R (R (R (handleCoercible other)))))
  {-# INLINE eff #-}


-- | Run a 'NonDet' effect, returning the first successful result in an 'Alternative' functor.
--
--   Unlike 'runNonDet', this will terminate immediately upon finding a solution.
--
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) === [a]
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) === Just a
runNonDetOnce :: (Alternative f, Carrier sig m, Effect sig) => OnceC m a -> m (f a)
runNonDetOnce = runNonDet . runCull . cull . runOnceC

newtype OnceC m a = OnceC { runOnceC :: CullC (NonDetC m) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadPlus)

instance (Carrier sig m, Effect sig) => Carrier (Empty :+: Choose :+: sig) (OnceC m) where
  eff = OnceC . eff . R . R . R . handleCoercible
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.NonDet
-- >>> import Control.Effect.Pure
-- >>> import Data.Foldable (asum)
