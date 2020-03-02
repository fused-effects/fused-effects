{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides the 'Effect' class that many effect types implement.
--
-- @since 1.0.0.0
module Control.Effect.Class
( Effect(..)
-- * Generic deriving of 'Effect' instances.
, GEffect(..)
) where

import Data.Coerce
import GHC.Generics

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the carrier’s suspended context.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'thread' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class Effect sig where
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
    :: (Functor ctx, Monad m)
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to thread the handler through.
    -> sig n (ctx a)
  default thread
    :: (Functor ctx, Monad m, Generic1 (sig m), Generic1 (sig n), GEffect m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  thread ctx handler = to1 . gthread ctx handler . from1
  {-# INLINE thread #-}


-- | Generic implementation of 'Effect'.
class GEffect m m' rep rep' where
  -- | Generic implementation of 'thread'.
  gthread
    :: (Functor ctx, Monad m)
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GEffect m m' rep rep' => GEffect m m' (M1 i c rep) (M1 i c rep') where
  gthread ctx handler = M1 . gthread ctx handler . unM1
  {-# INLINE gthread #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :+: r) (l' :+: r') where
  gthread ctx handler (L1 l) = L1 (gthread ctx handler l)
  gthread ctx handler (R1 r) = R1 (gthread ctx handler r)
  {-# INLINE gthread #-}

instance (GEffect m m' l l', GEffect m m' r r') => GEffect m m' (l :*: r) (l' :*: r') where
  gthread ctx handler (l :*: r) = gthread ctx handler l :*: gthread ctx handler r
  {-# INLINE gthread #-}

instance GEffect m m' V1 V1 where
  gthread _ _ v = case v of {}
  {-# INLINE gthread #-}

instance GEffect m m' U1 U1 where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance GEffect m m' (K1 R c) (K1 R c) where
  gthread _ _ = coerce
  {-# INLINE gthread #-}

instance GEffect m m' Par1 Par1 where
  gthread ctx _ = Par1 . (<$ ctx) . unPar1
  {-# INLINE gthread #-}

instance (Functor f, GEffect m m' g g') => GEffect m m' (f :.: g) (f :.: g') where
  gthread ctx handler = Comp1 . fmap (gthread ctx handler) . unComp1
  {-# INLINE gthread #-}

instance GEffect m m' (Rec1 m) (Rec1 m') where
  gthread ctx handler = Rec1 . handler . (<$ ctx) . unRec1
  {-# INLINE gthread #-}

instance Effect sig => GEffect m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  gthread ctx handler = Rec1 . thread ctx handler . unRec1
  {-# INLINE gthread #-}
