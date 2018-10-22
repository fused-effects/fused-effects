{-# LANGUAGE DefaultSignatures, FunctionalDependencies, RankNTypes #-}
module Control.Effect.Carrier
( HFunctor(..)
, Effect(..)
, Carrier(..)
, handlePure
) where

class HFunctor h where
  -- | Functor map. This is required to be 'fmap'.
  --
  --   This can go away once we have quantified constraints.
  fmap' :: (a -> b) -> (h m a -> h m b)
  default fmap' :: Functor (h m) => (a -> b) -> (h m a -> h m b)
  fmap' = fmap

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hmap :: (forall x . m x -> n x) -> (h m a -> h n a)


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: Functor f
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (m a)
         -> sig n (n (f a))


-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'ret' and 'eff' methods.
class HFunctor sig => Carrier sig h | h -> sig where
  -- | Wrap a return value.
  ret :: a -> h a

  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig h (h a) -> h a


-- | Apply a handler specified as a natural transformation to both higher-order and continuation positions within an 'HFunctor'.
handlePure :: HFunctor sig => (forall x . f x -> g x) -> sig f (f a) -> sig g (g a)
handlePure handler = hmap handler . fmap' handler
{-# INLINE handlePure #-}
