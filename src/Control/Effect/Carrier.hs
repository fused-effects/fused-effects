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
  -- | Functor map. This is required to be 'fmap'.
  --
  --   This can go away once we have quantified constraints.
  fmap' :: (a -> b) -> (h m a -> h m b)
  default fmap' :: Functor (h m) => (a -> b) -> (h m a -> h m b)
  fmap' = fmap
  {-# INLINE fmap' #-}

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  -- A definition for 'hmap' over first-order effects can be derived with the @-XDeriveAnyClass@ extension.
  hmap :: (forall x . m x -> n x) -> (h m a -> h n a)

  default hmap :: Coercible (h m a) (h n a)
               => (forall x . m x -> n x)
               -> (h m a -> h n a)
  hmap _ = coerce
  {-# INLINE hmap #-}


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
--
-- All first-order effects (those without recursive occurrences of @m@) admit a default definition
-- of 'handle'. The @-XDeriveAnyClass@ extension allows derivation of both 'HFunctor' and 'Effect':
--
-- @
--   data State s (m :: * -> *) k
--     = Get (s -> k)
--     | Put s k
--       deriving (Functor, HFunctor, Effect)
-- @
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all the way through to the continuation.
  handle :: Functor f
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (m a)
         -> sig n (n (f a))

  default handle :: (Functor f, Coercible (sig m (n (f a))) (sig n (n (f a))))
                 => f ()
                 -> (forall x . f (m x) -> n (f x))
                 -> sig m (m a)
                 -> sig n (n (f a))
  handle state handler = coerce . fmap' (handler . (<$ state))
  {-# INLINE handle #-}



-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m (m a) -> m a

-- | Apply a handler specified as a natural transformation to both higher-order and continuation positions within an 'HFunctor'.
handlePure :: HFunctor sig => (forall x . f x -> g x) -> sig f (f a) -> sig g (g a)
handlePure handler = hmap handler . fmap' handler
{-# INLINE handlePure #-}

-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
handleCoercible :: (HFunctor sig, Coercible f g) => sig f (f a) -> sig g (g a)
handleCoercible = handlePure coerce
{-# INLINE handleCoercible #-}
