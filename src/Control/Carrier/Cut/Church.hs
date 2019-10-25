{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A carrier for 'Cut' and 'NonDet' effects used in tandem (@Cut :+: NonDet@).
--
-- @since 1.0.0.0
module Control.Carrier.Cut.Church
( -- * Cut carrier
  runCut
, runCutA
, runCutM
, CutC(..)
  -- * Cut effect
, module Control.Effect.Cut
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Algebra
import Control.Effect.Cut
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cut' effect with continuations respectively interpreting 'pure' / '<|>', 'empty', and 'cutfail'.
--
-- @since 1.0.0.0
runCut :: (a -> m b -> m b) -> m b -> m b -> CutC m a -> m b
runCut cons nil fail (CutC runCutC) = runCutC cons nil fail

-- | Run a 'Cut' effect, returning all its results in an 'Alternative' collection.
--
-- @since 1.0.0.0
runCutA :: (Alternative f, Applicative m) => CutC m a -> m (f a)
runCutA = runCut (fmap . (<|>) . pure) (pure empty) (pure empty)

-- | Run a 'Cut' effect, mapping results into a 'Monoid'.
--
-- @since 1.0.0.0
runCutM :: (Applicative m, Monoid b) => (a -> b) -> CutC m a -> m b
runCutM leaf = runCut (fmap . mappend . leaf) (pure mempty) (pure mempty)

-- | @since 1.0.0.0
newtype CutC m a = CutC (forall b . (a -> m b -> m b) -> m b -> m b -> m b)
  deriving (Functor)

instance Applicative (CutC m) where
  pure a = CutC (\ cons nil _ -> cons a nil)
  {-# INLINE pure #-}
  CutC f <*> CutC a = CutC $ \ cons nil fail ->
    f (\ f' fs -> a (cons . f') fs fail) nil fail
  {-# INLINE (<*>) #-}

instance Alternative (CutC m) where
  empty = CutC (\ _ nil _ -> nil)
  {-# INLINE empty #-}
  CutC l <|> CutC r = CutC (\ cons nil fail -> l cons (r cons nil fail) fail)
  {-# INLINE (<|>) #-}

instance Monad (CutC m) where
  CutC a >>= f = CutC $ \ cons nil fail ->
    a (\ a' as -> runCut cons as fail (f a')) nil fail
  {-# INLINE (>>=) #-}

instance Fail.MonadFail m => Fail.MonadFail (CutC m) where
  fail s = lift (Fail.fail s)
  {-# INLINE fail #-}

-- | A single fixpoint is shared between all branches.
instance MonadFix m => MonadFix (CutC m) where
  mfix f = CutC $ \ cons nil fail -> mfix
    (toCut . f . run . fromCut)
    >>= run . runCut (fmap . cons) (pure nil) (pure fail) where
    toCut = runCut (fmap . (<|>) . pure) (pure empty) (pure cutfail)
    fromCut = runCut (<$) (error "mfix CutC: empty") (error "mfix CutC: cutfail")
  {-# INLINE mfix #-}

instance MonadIO m => MonadIO (CutC m) where
  liftIO io = lift (liftIO io)
  {-# INLINE liftIO #-}

instance MonadPlus (CutC m)

instance MonadTrans CutC where
  lift m = CutC (\ cons nil _ -> m >>= flip cons nil)
  {-# INLINE lift #-}

instance (Algebra sig m, CanThread sig (CutC m)) => Algebra (Cut :+: NonDet :+: sig) (CutC m) where
  type Context (CutC m) = CutC m
  alg (L Cutfail)    = CutC $ \ _    _   fail -> fail
  alg (L (Call m k)) = CutC $ \ cons nil fail -> runCut (\ a as -> runCut cons as fail (k a)) nil nil m
  alg (R (L (L Empty)))      = empty
  alg (R (L (R (Choose k)))) = k True <|> k False
  alg (R (R other))          = CutC $ \ cons nil fail -> handle (pure ()) (runCut (fmap . (<|>)) (pure empty) (pure cutfail)) other >>= runCut cons nil fail
  {-# INLINE alg #-}
