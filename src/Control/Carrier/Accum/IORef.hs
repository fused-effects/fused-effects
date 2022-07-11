{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A carrier for 'Accum' effects.
This carrier performs its append operations strictly and thus avoids the space leaks inherent in lazy writer monads.
These appends are left-associative; as such, @[]@ is a poor choice of monoid for computations that entail many calls to 'add'.
The [Seq](http://hackage.haskell.org/package/containersdocs/Data-Sequence.html) or [DList](http://hackage.haskell.org/package/dlist) monoids may be a superior choice.
This carrier also uses an 'IORef' to store its accumulator, which allows it a 'MonadUnliftIO' instance, but precludes backtracking when run in conjunction with 'Control.Effect.NonDet'.

-- | @since 1.1.2.0
-}

module Control.Carrier.Accum.IORef
( -- * Accum carrier
  runAccum
, execAccum
, evalAccum
, AccumC(AccumC)
  -- * Accum effect
, module Control.Effect.Accum
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Accum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import qualified Data.Semigroup as S
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Carrier.Reader

-- | Run an 'Accum' effect with a 'Monoid'al log.
--
-- @
-- 'runAccum' w0 ('pure' a) = 'pure' (w0, a)
-- @
-- @
-- 'runAccum' w0 ('add' w) = 'pure' (w0 <> w, ())
-- @
-- @
-- 'runAccum' w0 ('add' w >> 'look') = 'pure' (w0 <> w, w0 <> w)
-- @
--
-- @since 1.1.2.0
runAccum :: MonadIO m => w -> AccumC w m a -> m (w, a)
runAccum start go = do
  ref <- liftIO (newIORef start)
  result <- runReader ref . runAccumC $ go
  final <- liftIO (readIORef ref)
  pure (final, result)
{-# INLINE runAccum #-}

-- | Run a 'Accum' effect (typically with a 'Monoid'al log),
--   producing the final log and discarding the result value.
--
-- @
-- 'execAccum' w = 'fmap' 'fst' . 'runAccum' w
-- @
--
-- @since 1.1.2.0
execAccum :: MonadIO m => w -> AccumC w m a -> m w
execAccum w = fmap fst . runAccum w
{-# INLINE execAccum #-}

-- | Run a 'Accum' effect (typically with a 'Monoid'al log),
--   producing the result value and discarding the final log.
--
-- @
-- 'evalAccum' w = 'fmap' 'snd' . 'runAccum' w
-- @
--
-- @since 1.1.2.0
evalAccum :: MonadIO m => w -> AccumC w m a -> m a
evalAccum w = fmap snd . runAccum w
{-# INLINE evalAccum #-}

-- | @since 1.1.2.0
newtype AccumC w m a = AccumC { runAccumC :: ReaderC (IORef w) m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans, MonadUnliftIO)

instance (Algebra sig m, S.Semigroup w, MonadIO m) => Algebra (Accum w :+: sig) (AccumC w m) where
  alg hdl sig ctx = case sig of
    L accum -> do
      ref <- AccumC (ask @(IORef w))
      (<$ ctx) <$> case accum of
        Add w' -> liftIO (modifyIORef' ref (S.<> w'))
        Look   -> liftIO (readIORef ref)
    R other  -> AccumC (alg (runAccumC . hdl) (R other) ctx)
  {-# INLINE alg #-}
