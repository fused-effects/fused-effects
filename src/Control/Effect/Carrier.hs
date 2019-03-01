{-# LANGUAGE DefaultSignatures, FunctionalDependencies, RankNTypes #-}
{-# LANGUAGE FlexibleContexts                                      #-}
{-# LANGUAGE FlexibleInstances                                     #-}
{-# LANGUAGE ScopedTypeVariables                                   #-}
{-# LANGUAGE TypeApplications                                      #-}
{-# LANGUAGE TypeOperators                                         #-}

module Control.Effect.Carrier
( HFunctor(..)
, Effect(..)
, Carrier(..)
, handlePure
, handleCoercible
, handleReader
, handleState
, handleEither
, handleTraversable
) where

import Control.Monad (join)
import Data.Coerce
import GHC.Generics

class HFunctor h where
  -- | Functor map. This is required to be 'fmap'.
  --
  --   This can go away once we have quantified constraints.
  fmap' :: (a -> b) -> (h m a -> h m b)
  default fmap' :: Functor (h m) => (a -> b) -> (h m a -> h m b)
  fmap' = fmap
  {-# INLINE fmap' #-}

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hmap :: (forall x . m x -> n x) -> (h m a -> h n a)
  default hmap :: (Generic (h m a), Generic (h n a), GHFunctor (Rep (h m a)) (Rep (h n a)) m n)
                => (forall x . m x -> n x) -> (h m a -> h n a)
  hmap f = to . ghmap f . from

class GHFunctor s t u v where
  ghmap :: (forall y. u y -> v y) -> s x -> t x

instance GHFunctor s t u v => GHFunctor (M1 _1 _2 s) (M1 _1 _2 t) u v where
  ghmap f u = M1 $ ghmap f $ unM1 u

instance (GHFunctor s t u v, GHFunctor s' t' u v) => GHFunctor (s :+: s') (t :+: t') u v where
  ghmap f (L1 x) = L1 $ ghmap f x
  ghmap f (R1 x) = R1 $ ghmap f x

instance (GHFunctor s t u v, GHFunctor s' t' u v) => GHFunctor (s :*: s') (t :*: t') u v where
  ghmap f (x :*: y) = ghmap f x :*: ghmap f y

instance GHFunctor U1 U1 u v where
  ghmap _ U1 = U1

instance GHFunctor V1 V1 u v where
  ghmap _ = id

instance {-# INCOHERENT #-} GHFunctor (Rec0 (u x)) (Rec0 (v x)) u v where
  ghmap f (K1 z) = K1 $ f z

instance {-# INCOHERENT #-} HFunctor l => GHFunctor (Rec0 (l u x)) (Rec0 (l v x)) u v where
  ghmap f (K1 z) = K1 $ hmap f z

instance {-# INCOHERENT #-} GHFunctor (K1 _1 z) (K1 _1 z) u v where
  ghmap _ (K1 z) = K1 z


-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended state.
class HFunctor sig => Effect sig where
  -- | Handle any effects in a signature by threading the carrier’s state all
  -- the way through to the continuation.
  handle :: Functor f
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (m a)
         -> sig n (n (f a))
  default handle
      :: ( Generic (sig m (m a))
         , Generic (sig n (n (f a)))
         , GEffect f (Rep (sig m (m a)))
                     (Rep (sig n (n (f a)))) m n
         )
      => f ()
      -> (forall x . f (m x) -> n (f x))
      -> sig m (m a)
      -> sig n (n (f a))
  handle s f = to . ghandle s f . from

class GEffect f s t u v where
  ghandle :: f ()
          -> (forall y. f (u y) -> v (f y))
          -> s x
          -> t x

instance GEffect f s t u v => GEffect f (M1 _1 _2 s) (M1 _1 _2 t) u v where
  ghandle s f u = M1 $ ghandle s f $ unM1 u

instance (GEffect f s t u v, GEffect f s' t' u v) => GEffect f (s :+: s') (t :+: t') u v where
  ghandle s f (L1 x) = L1 $ ghandle s f x
  ghandle s f (R1 x) = R1 $ ghandle s f x

instance (GEffect f s t u v, GEffect f s' t' u v) => GEffect f (s :*: s') (t :*: t') u v where
  ghandle s f (x :*: y) = ghandle s f x :*: ghandle s f y

instance GEffect f U1 U1 u v where
  ghandle _ _ U1 = U1

instance GEffect f V1 V1 u v where
  ghandle _ _ _ = undefined

instance {-# INCOHERENT #-} (Functor f, Functor n) => GEffect f (Rec0 (m (m a))) (Rec0 (n (n (f a)))) m n where
  ghandle s f (K1 z) = K1 $ fmap f $ f $ z <$ s

-- instance {-# INCOHERENT #-} (Functor f, Functor n, HFunctor l) => GEffect f (Rec0 (l m (m a))) (Rec0 (l n (n (f a)))) m n where
--   ghandle s f (K1 z) = K1 $ hmaphmap  _ $ fmap' (f . (<$ s)) $ z

instance {-# INCOHERENT #-} (Functor f) => GEffect f (Rec0 (s -> m a)) (Rec0 (s -> n (f a))) m n where
  ghandle s f (K1 z) = K1 $ fmap (f . (<$ s)) $ z

instance {-# INCOHERENT #-} GEffect f (K1 _1 a) (K1 _1 a) u v where
  ghandle _ _ (K1 z) = K1 z

instance {-# INCOHERENT #-} Functor f => GEffect f (K1 _1 (u a)) (K1 _1 (v (f a))) u v where
  ghandle s f (K1 z) = K1 $ f $ z <$ s




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

-- | Thread a 'Coercible' carrier through an 'HFunctor'.
--
--   This is applicable whenever @f@ is 'Coercible' to @g@, e.g. simple @newtype@s.
handleCoercible :: (HFunctor sig, Coercible f g) => sig f (f a) -> sig g (g a)
handleCoercible = handlePure coerce
{-# INLINE handleCoercible #-}

-- | Thread a @Reader@-like carrier through an 'HFunctor'.
handleReader :: HFunctor sig => r -> (forall x . f x -> r -> g x) -> sig f (f a) -> sig g (g a)
handleReader r run = handlePure (flip run r)
{-# INLINE handleReader #-}

-- | Thread a @State@-like carrier through an 'Effect'.
handleState :: Effect sig => s -> (forall x . f x -> s -> g (s, x)) -> sig f (f a) -> sig g (g (s, a))
handleState s run = handle (s, ()) (uncurry (flip run))
{-# INLINE handleState #-}

-- | Thread a carrier producing 'Either's through an 'Effect'.
handleEither :: (Carrier sig g, Effect sig) => (forall x . f x -> g (Either e x)) -> sig f (f a) -> sig g (g (Either e a))
handleEither run = handle (Right ()) (either (ret . Left) run)
{-# INLINE handleEither #-}

-- | Thread a carrier producing values in a 'Traversable' 'Monad' (e.g. '[]') through an 'Effect'.
handleTraversable :: (Effect sig, Applicative g, Monad m, Traversable m) => (forall x . f x -> g (m x)) -> sig f (f a) -> sig g (g (m a))
handleTraversable run = handle (pure ()) (fmap join . traverse run)
{-# INLINE handleTraversable #-}
