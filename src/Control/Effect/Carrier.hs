{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleInstances, FunctionalDependencies, RankNTypes, UndecidableInstances #-}
module Control.Effect.Carrier
( HFunctor(..)
, Effect(..)
, Carrier(..)
, handlePure
, handleCoercible
) where

import Data.Coerce

class HFunctor h where
  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hmap :: (Functor m, Functor n) => (forall x . m x -> n x) -> (h m a -> h n a)


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: (Functor f, Monad m, Monad n)
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m a
         -> sig n (f a)


-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a

-- | Apply a handler specified as a natural transformation to both higher-order and continuation positions within an 'HFunctor'.
handlePure :: (HFunctor sig, Functor f, Functor g) => (forall x . f x -> g x) -> sig f a -> sig g a
handlePure handler = hmap handler
{-# INLINE handlePure #-}

-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
handleCoercible :: (HFunctor sig, Functor f, Functor g, Coercible f g) => sig f a -> sig g a
handleCoercible = handlePure coerce
{-# INLINE handleCoercible #-}
