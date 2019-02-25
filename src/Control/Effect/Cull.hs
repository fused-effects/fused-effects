{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cull
( Cull(..)
, cull
, runCull
, CullC(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.NonDet.Internal
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

-- | 'Cull' effects are used with 'NonDet' to provide control over branching.
data Cull m k
  = forall a . Cull (m a) (a -> k)

deriving instance Functor (Cull m)

instance HFunctor Cull where
  hmap f (Cull m k) = Cull (f m) k
  {-# INLINE hmap #-}

instance Effect Cull where
  handle state handler (Cull m k) = Cull (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Cull nondeterminism in the argument, returning at most one result.
--
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b)))) == [a]
--   prop> run (runNonDet (runCull (cull (pure a <|> pure b) <|> pure c))) == [a, c]
--   prop> run (runNonDet (runCull (cull (asum (map pure (repeat a)))))) == [a]
cull :: (Carrier sig m, Member Cull sig) => m a -> m a
cull m = send (Cull m pure)


-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) == [a, b]
runCull :: (Alternative m, Carrier sig m) => CullC m a -> m a
runCull = (>>= runBranch (const empty)) . runReader False . runCullC

newtype CullC m a = CullC { runCullC :: ReaderC Bool m (Branch m () a) }
  deriving (Functor)

instance Alternative m => Applicative (CullC m) where
  pure = CullC . pure . Pure
  CullC f <*> CullC a = CullC (liftA2 (<*>) f a)

instance (Alternative m, Carrier sig m, Effect sig) => Alternative (CullC m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance (Alternative m, Monad m) => Monad (CullC m) where
  CullC m >>= f = CullC (m >>= \case
    None e    -> pure (None e)
    Pure a    -> runCullC (f a)
    Alt m1 m2 -> ReaderC (\ cull -> let k = runReader cull . runCullC . f in (m1 >>= k) <|> (m2 >>= k)))

instance (Alternative m, MonadFail m) => MonadFail (CullC m) where
  fail s = CullC (fail s)

instance (Alternative m, MonadIO m) => MonadIO (CullC m) where
  liftIO io = CullC (Pure <$> liftIO io)

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  eff op = CullC (ReaderC (\ cull -> handleSum (handleSum
    (eff . handle (Pure ()) (bindBranch (runReader cull . runCullC)))
    (\case
      Empty       -> pure (None ())
      Choose k    -> runReader cull (runCullC (k True)) >>= branch (const (runReader cull (runCullC (k False)))) (if cull then pure . Pure else \ a -> pure (Alt (pure a) (runReader cull (runCullC (k False)) >>= runBranch (const empty)))) (fmap pure . Alt)))
    (\ (Cull m k) -> runReader True (runCullC m) >>= bindBranch (runReader cull . runCullC . k))
    op))
    where bindBranch :: (Alternative m, Carrier sig m) => (b -> m (Branch m () a)) -> Branch m () b -> m (Branch m () a)
          bindBranch bind = branch (const (pure (None ()))) bind (\ a b -> pure (Alt (a >>= bind >>= runBranch (const empty)) (b >>= bind >>= runBranch (const empty))))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.NonDet
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
