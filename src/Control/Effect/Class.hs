{-# LANGUAGE ConstraintKinds, DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Effect(..)
, hmap
  -- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import Data.Coerce
import Data.Functor.Identity
import Data.Kind (Constraint)
import GHC.Generics

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended context.
--
-- Effects may additionally constrain the types of context that they support threading through their actions by defining an instance of the associated 'CanThread' type family. If no definition is given, it defaults to 'Functor'.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'thread' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class CanThread sig Identity => Effect sig where
  -- | Constrain the type of context algebras can pass to 'thread'.
  --
  -- Defaults to 'Functor'. Some effects may require a more restrictive constraint to thread handlers through. It is recommended, but not enforced, that imposed constraints be subclasses of 'Functor' wherever possible to ensure compatibility with the broadest variety of algebras.
  type CanThread sig (ctx :: (* -> *)) :: Constraint
  type CanThread sig ctx = Functor ctx

  -- | Handle any effects in a signature by threading the algebra’s handler all the way through to the continuation, starting from some initial context.
  --
  -- The handler is required to adhere to the following laws:
  --
  -- @
  -- handler . 'fmap' 'pure' = 'pure'
  -- @
  -- @
  -- handler . 'fmap' (k '=<<') = handler . 'fmap' k 'Control.Monad.<=<' handler
  -- @
  --
  -- respectively expressing that the handler does not alter the context of pure computations, and that the handler distributes over monadic composition.
  thread
    :: (Monad m, Monad n, CanThread sig ctx)
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to thread the handler through.
    -> sig n (ctx a)
  default thread
    :: (Monad m, Monad n, Generic1 (sig m), Generic1 (sig n), GEffect ctx m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  thread state handler = to1 . gthread state handler . from1
  {-# INLINE thread #-}


hmap
  :: (Effect sig, Monad m, Monad n, Functor (sig n))
  => (forall a . m a -> n a)
  -> sig m a
  -> sig n a
hmap f = fmap runIdentity . thread (Identity ()) (fmap Identity . f . runIdentity)


-- | Generic implementation of 'Effect'.
class GEffect ctx m m' rep rep' where
  -- | Generic implementation of 'thread'.
  gthread
    :: (Monad m, Monad m')
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GEffect ctx m m' rep rep' => GEffect ctx m m' (M1 i c' rep) (M1 i c' rep') where
  gthread state handler = M1 . gthread state handler . unM1
  {-# INLINE gthread #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :+: r) (l' :+: r') where
  gthread state handler (L1 l) = L1 (gthread state handler l)
  gthread state handler (R1 r) = R1 (gthread state handler r)
  {-# INLINE gthread #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :*: r) (l' :*: r') where
  gthread state handler (l :*: r) = gthread state handler l :*: gthread state handler r
  {-# INLINE gthread #-}

instance GEffect ctx m m' V1 V1 where
  gthread _ _ v = case v of {}
  {-# INLINE gthread #-}

instance GEffect ctx m m' U1 U1 where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance GEffect ctx m m' (K1 R k) (K1 R k) where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance Functor ctx => GEffect ctx m m' Par1 Par1 where
  gthread state _ = Par1 . (<$ state) . unPar1
  {-# INLINE gthread #-}

instance (Functor g, GEffect ctx m m' h h') => GEffect ctx m m' (g :.: h) (g :.: h') where
  gthread state handler = Comp1 . fmap (gthread state handler) . unComp1
  {-# INLINE gthread #-}

instance Functor ctx => GEffect ctx m m' (Rec1 m) (Rec1 m') where
  gthread state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE gthread #-}

instance (Effect sig, CanThread sig ctx) => GEffect ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gthread state handler = Rec1 . thread state handler . unRec1
  {-# INLINE gthread #-}
