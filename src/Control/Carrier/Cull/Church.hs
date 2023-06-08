{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A carrier for 'Cull' and 'NonDet' effects used in tandem (@Cull :+: NonDet@).
--
-- @since 1.0.0.0
module Control.Carrier.Cull.Church
( -- * Cull carrier
  runCull
, runCullA
, runCullM
, CullC(CullC)
  -- * Cull effect
, module Control.Effect.Cull
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Algebra
import Control.Applicative
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Effect.Cull
import Control.Effect.NonDet
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cull' effect with continuations respectively interpreting '<|>', 'pure', and 'empty'. Branches outside of any 'cull' block will not be pruned.
--
-- @
-- runCull fork leaf nil ('pure' a '<|>' 'empty') = leaf a \`fork\` nil
-- @
-- @
-- runCull fork leaf nil ('cull' ('pure' a '<|>' b)) = leaf a
-- @
--
-- @since 1.0.0.0
runCull :: (m b -> m b -> m b) -> (a -> m b) -> m b -> CullC m a -> m b
runCull fork leaf nil (CullC m) = runNonDet fork leaf nil (runReader False m)
{-# INLINE runCull #-}

-- | Run a 'Cull' effect, interpreting the result into an 'Alternative' functor. Choice is handled with '<|>', embedding with 'pure', and failure with 'empty'.
--
-- @since 1.0.0.0
runCullA :: (Alternative f, Applicative m) => CullC m a -> m (f a)
runCullA = runCull (liftA2 (<|>)) (pure . pure) (pure empty)
{-# INLINE runCullA #-}

-- | Run a 'Cull' effect, mapping results into a 'Monoid'.
--
-- @since 1.0.0.0
runCullM :: (Applicative m, Monoid b) => (a -> b) -> CullC m a -> m b
runCullM leaf = runCull (liftA2 mappend) (pure . leaf) (pure mempty)
{-# INLINE runCullM #-}

-- | @since 1.0.0.0
newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadIO)

instance Alternative (CullC m) where
  empty = CullC empty
  {-# INLINE empty #-}

  CullC l <|> CullC r = CullC $ ReaderC $ \ cull ->
    if cull then
      NonDetC $ \ fork leaf nil ->
        runNonDet fork leaf (runNonDet fork leaf nil (runReader cull r)) (runReader cull l)
    else
      runReader cull l <|> runReader cull r
  {-# INLINE (<|>) #-}

-- | Separate fixpoints are computed for each branch.
deriving instance MonadFix m => MonadFix (CullC m)

instance MonadPlus (CullC m)

instance MonadTrans CullC where
  lift = CullC . lift . lift
  {-# INLINE lift #-}

instance Algebra sig m => Algebra (Cull :+: NonDet :+: sig) (CullC m) where
  alg hdl sig ctx = case sig of
    L (Cull m)       -> CullC (local (const True) (runCullC (hdl (m <$ ctx))))
    R (L (L Empty))  -> empty
    R (L (R Choose)) -> pure (True <$ ctx) <|> pure (False <$ ctx)
    R (R other)      -> CullC (alg (runCullC . hdl) (R (R other)) ctx)
  {-# INLINE alg #-}
