{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cull where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet
import Control.Effect.Sum

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
cull m = send (Cull m ret)


-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) == [a, b]
runCull :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (CullC m) a -> m a
runCull = (>>= runBranch (const empty)) . flip runCullC False . interpret

newtype CullC m a = CullC { runCullC :: Bool -> m (Branch m () a) }

instance (Alternative m, Carrier sig m, Effect sig, Monad m) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  ret = CullC . const . ret . Pure
  {-# INLINE ret #-}

  eff op = CullC (\ cull -> handleSum (handleSum
    (eff . handle (Pure ()) (bindBranch (flip runCullC cull)))
    (\case
      Empty       -> ret (None ())
      Choose k    -> runCullC (k True) cull >>= branch (const (runCullC (k False) cull)) (if cull then ret . Pure else \ a -> ret (Alt (ret a) (runCullC (k False) cull >>= runBranch (const empty)))) (fmap ret . Alt)))
    (\ (Cull m k) -> runCullC m True >>= bindBranch (flip runCullC cull . k))
    op)
    where bindBranch :: (Alternative m, Carrier sig m, Monad m) => (b -> m (Branch m () a)) -> Branch m () b -> m (Branch m () a)
          bindBranch bind = branch (const (ret (None ()))) bind (\ a b -> ret (Alt (a >>= bind >>= runBranch (const empty)) (b >>= bind >>= runBranch (const empty))))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Foldable (asum)
