{-# LANGUAGE EmptyCase, MultiParamTypeClasses #-}
module Control.Carrier.Pure
( -- * Pure effect
  module Control.Effect.Pure
  -- * Pure carrier
, run
, PureC(..)
) where

import Control.Applicative
import Control.Carrier.Class
import Control.Effect.Pure
import Control.Monad.Fix
import Data.Coerce
import Data.Functor.Classes

-- | Run an action exhausted of effects to produce its final result value.
run :: PureC a -> a
run = runPureC
{-# INLINE run #-}

newtype PureC a = PureC { runPureC :: a }
  deriving (Eq, Ord, Show)

instance Functor PureC where
  fmap = coerce
  {-# INLINE fmap #-}

  a <$ _ = pure a
  {-# INLINE (<$) #-}

instance Applicative PureC where
  pure = PureC
  {-# INLINE pure #-}

  (<*>) = coerce
  {-# INLINE (<*>) #-}

  liftA2 = coerce
  {-# INLINE liftA2 #-}

  _ *> b = b
  {-# INLINE (*>) #-}

  a <* _ = a
  {-# INLINE (<*) #-}

instance Monad PureC where
  return = pure
  {-# INLINE return #-}

  PureC a >>= f = f a
  {-# INLINE (>>=) #-}

instance MonadFix PureC where
  mfix f = PureC (fix (runPureC . f))
  {-# INLINE mfix #-}

instance Carrier Pure PureC where
  eff v = case v of {}
  {-# INLINE eff #-}


instance Eq1 PureC where
  liftEq eq (PureC a1) (PureC a2) = a1 `eq` a2

instance Ord1 PureC where
  liftCompare compare (PureC a1) (PureC a2) = a1 `compare` a2

instance Show1 PureC where
  liftShowsPrec sp _ d = showsUnaryWith sp "PureC" d . run
