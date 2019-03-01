{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cull
( Cull(..)
, cull
, runCull
, CullC(..)
, runNonDetOnce
, OnceC(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as MT
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

instance (Alternative m, Carrier sig m) => Alternative (CullC m) where
  empty = CullC (pure (None ()))
  l <|> r = CullC $ do
    res <- runCullC l
    case res of
      None _ -> runCullC r
      Pure a -> do
        cull <- ask
        if cull then
          pure res
        else
          pure (Alt (pure a) (runReader cull (runCullC r) >>= runBranch (const empty)))
      Alt _ _ -> pure res

instance (Alternative m, Monad m) => Monad (CullC m) where
  CullC m >>= f = CullC (m >>= \case
    None e    -> pure (None e)
    Pure a    -> runCullC (f a)
    Alt m1 m2 -> ReaderC (MT.ReaderT (\ cull -> let k = runReader cull . runCullC . f in (m1 >>= k) <|> (m2 >>= k))))

instance (Alternative m, MonadFail m) => MonadFail (CullC m) where
  fail s = CullC (fail s)

instance (Alternative m, MonadIO m) => MonadIO (CullC m) where
  liftIO io = CullC (Pure <$> liftIO io)

instance (Alternative m, Carrier sig m) => MonadPlus (CullC m)

instance MonadTrans CullC where
  lift m = CullC (lift (Pure <$> m))

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  eff (L (Cull m k))     = CullC (local (const True) (runCullC m) >>= bindBranch (runCullC . k))
  eff (R (L Empty))      = empty
  eff (R (L (Choose k))) = k True <|> k False
  eff (R (R other))      = CullC (eff (R (handle (Pure ()) (bindBranch runCullC) other)))
  {-# INLINE eff #-}

bindBranch :: (Alternative m, Carrier sig m) => (b -> ReaderC Bool m (Branch m () a)) -> Branch m () b -> ReaderC Bool m (Branch m () a)
bindBranch bind a = do
  cull <- ask
  branch (const (pure (None ()))) bind (\ a b -> pure (Alt (a >>= runReader cull . bind >>= runBranch (const empty)) (b >>= runReader cull . bind >>= runBranch (const empty)))) a


-- | Run a 'NonDet' effect, returning the first successful result in an 'Alternative' functor.
--
--   Unlike 'runNonDet', this will terminate immediately upon finding a solution.
--
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == [a]
--   prop> run (runNonDetOnce (asum (map pure (repeat a)))) == Just a
runNonDetOnce :: (Alternative f, Carrier sig m, Effect sig) => OnceC m a -> m (f a)
runNonDetOnce = runNonDet . runCull . cull . runOnceC

newtype OnceC m a = OnceC { runOnceC :: CullC (BTreeC m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

deriving instance (Carrier sig m, Effect sig) => Alternative (OnceC m)
deriving instance (Carrier sig m, Effect sig) => MonadPlus (OnceC m)

instance MonadTrans OnceC where
  lift m = OnceC (CullC (ReaderC (MT.ReaderT (const (BTreeC (\ _ pur _ -> m >>= pur . Pure))))))

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (OnceC m) where
  eff = OnceC . eff . R . R . handleCoercible


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.NonDet
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
