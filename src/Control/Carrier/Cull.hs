{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Cull
( -- * Cull effect
  module Control.Effect.Cull
  -- * Cull carrier
, runCull
, CullC(..)
  -- * Re-exports
, Carrier
, Member
, run
) where

import Control.Applicative (Alternative(..))
import Control.Carrier.Class
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Control.Effect.Cull
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Cull' effect. Branches outside of any 'cull' block will not be pruned.
--
--   prop> run (runNonDet (runCull (pure a <|> pure b))) === [a, b]
runCull :: Alternative m => CullC m a -> m a
runCull (CullC m) = runNonDetC (runReader False m) (<|>) pure empty

newtype CullC m a = CullC { runCullC :: ReaderC Bool (NonDetC m) a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO)

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
