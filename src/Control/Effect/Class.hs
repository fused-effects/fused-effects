{-# LANGUAGE ConstraintKinds, DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}

-- | Provides the 'Effect' class that effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( -- * 'Effect' class
  Effect(..)
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
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'thread' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class c Identity => Effect (c :: (* -> *) -> Constraint) sig | sig -> c where
  -- | Handle any effects in a signature by threading the algebra’s handler all the way through to the continuation, starting from some initial context.
  --
  -- The handler is expressed as a /distributive law/, and required to adhere to the following laws:
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
    :: (Monad m, Monad n, c ctx)
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
  thread ctx handler = to1 . gthread ctx handler . from1
  {-# INLINE thread #-}


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
  gthread ctx handler = M1 . gthread ctx handler . unM1
  {-# INLINE gthread #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :+: r) (l' :+: r') where
  gthread ctx handler (L1 l) = L1 (gthread ctx handler l)
  gthread ctx handler (R1 r) = R1 (gthread ctx handler r)
  {-# INLINE gthread #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :*: r) (l' :*: r') where
  gthread ctx handler (l :*: r) = gthread ctx handler l :*: gthread ctx handler r
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
  gthread ctx _ = Par1 . (<$ ctx) . unPar1
  {-# INLINE gthread #-}

instance (Functor g, GEffect ctx m m' h h') => GEffect ctx m m' (g :.: h) (g :.: h') where
  gthread ctx handler = Comp1 . fmap (gthread ctx handler) . unComp1
  {-# INLINE gthread #-}

instance Functor ctx => GEffect ctx m m' (Rec1 m) (Rec1 m') where
  gthread ctx handler = Rec1 . handler . (<$ ctx) . unRec1
  {-# INLINE gthread #-}

instance (Effect c sig, c ctx) => GEffect ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gthread ctx handler = Rec1 . thread ctx handler . unRec1
  {-# INLINE gthread #-}
