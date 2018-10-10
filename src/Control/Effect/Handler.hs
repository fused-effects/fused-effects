{-# LANGUAGE DefaultSignatures, FunctionalDependencies, RankNTypes #-}
module Control.Effect.Handler where

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class Effect sig where
  -- | Functor map. This is required to be 'fmap'.
  fmap' :: (a -> b) -> (sig m a -> sig m b)
  default fmap' :: Functor (sig m) => (a -> b) -> (sig m a -> sig m b)
  fmap' = fmap

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hfmap :: (forall x . m x -> n x) -> sig m a -> sig n a

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
