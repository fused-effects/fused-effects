{-# LANGUAGE ConstraintKinds, DefaultSignatures, EmptyCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}

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
-- Effects may additionally constrain the types of context that they support threading through their actions by defining an instance of the associated 'CanHandle' type family. If no definition is given, it defaults to 'Functor'.
--
-- All first-order effects (those without existential occurrences of @m@) admit a default definition of 'handle' provided a 'Generic1' instance is available for the effect.
--
-- @since 1.0.0.0
class CanHandle sig Identity => Effect sig where
  -- | Constrain the type of context algebras can pass to 'handle'.
  --
  -- Defaults to 'Functor'. Some effects may require a more restrictive constraint to thread handlers through. It is recommended, but not enforced, that imposed constraints be subclasses of 'Functor' wherever possible to ensure compatibility with the broadest variety of algebras.
  type CanHandle sig (ctx :: (* -> *)) :: Constraint
  type CanHandle sig ctx = Functor ctx

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
  handle
    :: (Monad m, Monad n, CanHandle sig ctx)
    => ctx ()                              -- ^ The initial context.
    -> (forall x . ctx (m x) -> n (ctx x)) -- ^ A handler for actions in a context, producing actions with a derived context.
    -> sig m a                             -- ^ The effect to thread the handler through.
    -> sig n (ctx a)
  default handle
    :: (Monad m, Monad n, Generic1 (sig m), Generic1 (sig n), GEffect ctx m n (Rep1 (sig m)) (Rep1 (sig n)))
    => ctx ()
    -> (forall x . ctx (m x) -> n (ctx x))
    -> sig m a
    -> sig n (ctx a)
  handle state handler = to1 . ghandle state handler . from1
  {-# INLINE handle #-}


-- | Generic implementation of 'Effect'.
class GEffect ctx m m' rep rep' where
  -- | Generic implementation of 'handle'.
  ghandle
    :: (Monad m, Monad m')
    => ctx ()
    -> (forall x . ctx (m x) -> m' (ctx x))
    -> rep a
    -> rep' (ctx a)

instance GEffect ctx m m' rep rep' => GEffect ctx m m' (M1 i c' rep) (M1 i c' rep') where
  ghandle state handler = M1 . ghandle state handler . unM1
  {-# INLINE ghandle #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :+: r) (l' :+: r') where
  ghandle state handler (L1 l) = L1 (ghandle state handler l)
  ghandle state handler (R1 r) = R1 (ghandle state handler r)
  {-# INLINE ghandle #-}

instance (GEffect ctx m m' l l', GEffect ctx m m' r r') => GEffect ctx m m' (l :*: r) (l' :*: r') where
  ghandle state handler (l :*: r) = ghandle state handler l :*: ghandle state handler r
  {-# INLINE ghandle #-}

instance GEffect ctx m m' V1 V1 where
  ghandle _ _ v = case v of {}
  {-# INLINE ghandle #-}

instance GEffect ctx m m' U1 U1 where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance GEffect ctx m m' (K1 R k) (K1 R k) where
  ghandle _ _ = coerce
  {-# INLINE ghandle #-}

instance Functor ctx => GEffect ctx m m' Par1 Par1 where
  ghandle state _ = Par1 . (<$ state) . unPar1
  {-# INLINE ghandle #-}

instance (Functor g, GEffect ctx m m' h h') => GEffect ctx m m' (g :.: h) (g :.: h') where
  ghandle state handler = Comp1 . fmap (ghandle state handler) . unComp1
  {-# INLINE ghandle #-}

instance Functor ctx => GEffect ctx m m' (Rec1 m) (Rec1 m') where
  ghandle state handler = Rec1 . handler . (<$ state) . unRec1
  {-# INLINE ghandle #-}

instance (Effect sig, CanHandle sig ctx) => GEffect ctx m m' (Rec1 (sig m)) (Rec1 (sig m')) where
  ghandle state handler = Rec1 . handle state handler . unRec1
  {-# INLINE ghandle #-}
