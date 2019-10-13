{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
{- | A carrier for 'NonDet' effects providing choice and failure.

This carrier terminates immediately upon finding a successful result.
-}

module Control.Carrier.NonDet.Maybe
( -- * NonDet carrier
  runNonDet
, NonDetC(..)
  -- * NonDet effects
, module Control.Effect.NonDet
) where

import Control.Carrier
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- | Run 'NonDet' effects, returning 'Nothing' for empty computations, or 'Just' the result otherwise. Note that this will terminate on the first successful result.
--
-- @
-- 'runNonDet' 'empty' = pure 'Nothing'
-- 'runNonDet' ('pure' a) = 'pure' ('Just' a)
-- 'runNonDet' ('foldMapA' 'pure' [1..]) = 'pure' ('Just' 1)
-- @
--
-- @since 1.0.0.0
runNonDet :: NonDetC m a -> m (Maybe a)
runNonDet = runMaybeT . runNonDetC
{-# INLINE runNonDet #-}

newtype NonDetC m a = NonDetC { runNonDetC :: MaybeT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

-- | 'NonDetC' passes 'Fail.MonadFail' operations along to the underlying monad @m@, rather than interpreting it as a synonym for 'empty' à la 'MaybeT'.
--
-- @since 1.0.0.0
instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff = NonDetC . eff . handleCoercible
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
