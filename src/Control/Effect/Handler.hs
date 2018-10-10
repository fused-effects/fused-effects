{-# LANGUAGE DefaultSignatures, FunctionalDependencies, RankNTypes #-}
module Control.Effect.Handler where

class HFunctor h where
  -- | Functor map. This is required to be 'fmap'.
  --
  --   This can go away once we have quantified constraints.
  fmap' :: (a -> b) -> (h m a -> h m b)
  default fmap' :: Functor (h m) => (a -> b) -> (h m a -> h m b)
  fmap' = fmap

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hfmap :: (forall x . m x -> n x) -> h m a -> h n a


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in higher-order positions by threading the carrier’s state all the way through to the continuation.
  handle :: (Functor f, Monad n)
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (m a)
         -> sig n (n (f a))


class Effect sig => TermAlgebra h sig | h -> sig where
  var :: a -> h a
  con :: sig h (h a) -> h a

class (Monad m, TermAlgebra m sig) => TermMonad m sig | m -> sig
