{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Effect
( Eff(..)
, send
, fold
, foldH
, interpret
, interpret2
, interpretRest
, reinterpret
, reinterpret2
, reinterpretRest
, reinterpret_2
, reinterpret2_2
, reinterpretRest_2
, Effect(..)
, TermAlgebra(..)
, TermMonad
, Carrier(..)
, Void
, run
, (:+:)(..)
, (\/)
, Subset(..)
, Lift(..)
, NonDet(..)
, Fail(..)
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Monad (ap, liftM)
import Control.Monad.Codensity
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) (Eff effects a))

-- | The class of effect types, which must:
--
--   1. Be functorial in their last two arguments, and
--   2. Support threading effects in higher-order positions through using the 'Carrier'’s 'suspend'ed state.
class Effect sig where
  -- | Functor map. This is required to be 'fmap'.
  fmap' :: (a -> b) -> (sig m a -> sig m b)
  default fmap' :: Functor (sig m) => (a -> b) -> (sig m a -> sig m b)
  fmap' = fmap

  -- | Higher-order functor map of a natural transformation over higher-order positions within the effect.
  hfmap :: (forall x . m x -> n x) -> sig m a -> sig n a

  -- | Handle any effects in higher-order positions by threading the 'Carrier'’s state all the way through to the continuation.
  --
  --   For first-order effects (which don’t contain higher-order positions), this will simply involve repackaging the effect’s arguments at the new type.
  --
  --   For higher-order effects (which do contain higher-order positions), the 'Carrier' actions @c n@ must be 'resume'd to obtain the necessary @n@ action, and the state passed along to the continuation via 'resume' and 'wrap'.
  handle :: (Carrier f c, Monad n)
         => f ()
         -> sig (c n) (c n a)
         -> sig n (c n a)

class Effect sig => TermAlgebra h sig | h -> sig where
  var :: a -> h a
  con :: sig h (h a) -> h a

instance Effect sig => TermAlgebra (Eff sig) sig where
  var = Return
  con = Eff

instance TermAlgebra h sig => TermAlgebra (Codensity h) sig where
  var = pure
  con = algCod con

algCod :: TermAlgebra h sig
       => (forall a . sig h (h a) -> h a)
       -> (forall a . sig (Codensity h) (Codensity h a) -> Codensity h a)
algCod alg op = Codensity (\ k -> alg (hfmap (runCodensity var) (fmap' (runCodensity k) op)))


class (Monad m, TermAlgebra m sig) => TermMonad m sig | m -> sig

instance Effect sig => TermMonad (Eff sig) sig

instance TermAlgebra h sig => TermMonad (Codensity h) sig


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Subset effect sig, TermAlgebra m sig) => effect m (m a) -> m a
send = con . inj


-- | Fold a generator and first-order algebra over an 'Eff' to obtain some final result value.
fold :: Effect sig
     => (a -> b)
     -> (sig (Eff sig) b -> b)
     -> (Eff sig a -> b)
fold gen alg = go
  where go (Return x) = gen x
        go (Eff op)   = alg (fmap' go op)

-- | Fold a higher-order algebra over an 'Eff' to obtain some final result value.
foldH :: forall sig f
      .  Effect sig
      => (forall a . a -> f a)
      -> (forall a . sig f (f a) -> f a)
      -> (forall a . Eff sig a -> f a)
foldH gen alg = go
  where go :: Eff sig a -> f a
        go (Return x) = gen x
        go (Eff op)   = alg (hfmap go (fmap' go op))


-- | Interpret an 'Effect'’s requests into a 'Carrier' using the passed algebra.
interpret :: (Effect eff, Carrier f c, TermMonad m sig)
          => (forall a . eff (c m) (c m a) -> c m a)
          -> (forall a . Eff (eff :+: sig) a -> c m a)
interpret alg = foldH gen (alg \/ interpretRest)
{-# INLINE interpret #-}

-- | Interpret two 'Effect's’ requests into a 'Carrier' using the passed algebras.
interpret2 :: (Effect eff1, Effect eff2, Carrier f c, TermMonad m sig)
           => (forall a . eff1 (c m) (c m a) -> c m a)
           -> (forall a . eff2 (c m) (c m a) -> c m a)
           -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c m a)
interpret2 alg1 alg2 = foldH gen (alg1 \/ alg2 \/ interpretRest)
{-# INLINE interpret2 #-}

-- | Interpret any requests in higher-order positions in the remaining effects.
--
--   This is typically passed to 'foldH' as the last of a '\/'-chain of algebras, and can be used uniformly regardless of how many effects are being handled.
interpretRest :: (Carrier f c, TermMonad m sig)
              => sig (c m) (c m a)
              -> c m a
interpretRest op = suspend (\ state -> joinl (con (fmap' var (handle state op))))


-- | Reinterpret an 'Effect'’s requests into a 'Carrier' and requests of a new 'Effect' using the passed algebra.
reinterpret :: (Effect eff, Effect sig, Carrier f c, TermMonad m (new :+: sig))
            => (forall a . eff (c m) (c m a) -> c m a)
            -> (forall a . Eff (eff :+: sig) a -> c m a)
reinterpret alg = foldH gen (alg \/ reinterpretRest)
{-# INLINE reinterpret #-}

-- | Reinterpret two 'Effect's’ requests into a 'Carrier' and requests of a new 'Effect' using the passed algebras.
reinterpret2 :: (Effect eff1, Effect eff2, Effect sig, Carrier f c, TermMonad m (new :+: sig))
             => (forall a . eff1 (c m) (c m a) -> c m a)
             -> (forall a . eff2 (c m) (c m a) -> c m a)
             -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c m a)
reinterpret2 alg1 alg2 = foldH gen (alg1 \/ alg2 \/ reinterpretRest)
{-# INLINE reinterpret2 #-}

-- | Reinterpret any requests in higher-order positions in the remaining effects.
--
--   This is typically passed to 'foldH' as the last of a '\/'-chain of algebras, and can be used uniformly regardless of how many effects are being handled.
reinterpretRest :: (Effect sig, Carrier f c, TermMonad m (new :+: sig))
                => sig (c m) (c m a)
                -> c m a
reinterpretRest op = suspend (\ state -> joinl (con (fmap' var (R (handle state op)))))

-- | Reinterpret an 'Effect'’s requests into a 'Carrier' and requests of two new 'Effect's using the passed algebra.
reinterpret_2 :: (Effect eff, Effect sig, Carrier f c, TermMonad m (new1 :+: new2 :+: sig))
             => (forall a . eff (c m) (c m a) -> c m a)
             -> (forall a . Eff (eff :+: sig) a -> c m a)
reinterpret_2 alg = foldH gen (alg \/ reinterpretRest_2)
{-# INLINE reinterpret_2 #-}

-- | Reinterpret two 'Effect's’ requests into a 'Carrier' and requests of two new 'Effect's using the passed algebras.
reinterpret2_2 :: (Effect eff1, Effect eff2, Effect sig, Carrier f c, TermMonad m (new1 :+: new2 :+: sig))
             => (forall a . eff1 (c m) (c m a) -> c m a)
             -> (forall a . eff2 (c m) (c m a) -> c m a)
             -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c m a)
reinterpret2_2 alg1 alg2 = foldH gen (alg1 \/ alg2 \/ reinterpretRest_2)
{-# INLINE reinterpret2_2 #-}

-- | Reinterpret any requests in higher-order positions in the remaining effects.
--
--   This is typically passed to 'foldH' as the last of a '\/'-chain of algebras, and can be used uniformly regardless of how many effects are being handled.
reinterpretRest_2 :: (Effect sig, Carrier f c, TermMonad m (new1 :+: new2 :+: sig))
                => sig (c m) (c m a)
                -> c m a
reinterpretRest_2 op = suspend (\ state -> joinl (con (fmap' var (R (R (handle state op))))))


data Void m k
  deriving (Functor)

instance Effect Void where
  hfmap _ v = case v of {}
  handle _ v = case v of {}

-- | Run an 'Eff' exhausted of effects to produce its final result value.
run :: Codensity VoidH a -> a
run = runVoidH . runCodensity VoidH


newtype VoidH a = VoidH { runVoidH :: a }

instance TermAlgebra VoidH Void where
  var = VoidH
  con v = case v of {}


newtype Lift sig m k = Lift { unLift :: sig k }
  deriving (Functor)

instance Functor sig => Effect (Lift sig) where
  hfmap _ (Lift op) = Lift op

  handle _ (Lift op) = Lift op

instance Subset (Lift IO) sig => MonadIO (Eff sig) where
  liftIO = send . Lift . fmap pure


data (f :+: g) m k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Functor, Ord, Show)

infixr 4 :+:

instance (Effect l, Effect r) => Effect (l :+: r) where
  hfmap f (L l) = L (hfmap f l)
  hfmap f (R r) = R (hfmap f r)

  fmap' f (L l) = L (fmap' f l)
  fmap' f (R r) = R (fmap' f r)

  handle state (L l) = L (handle state l)
  handle state (R r) = R (handle state r)

-- | Lift algebras for either side of a sum into a single algebra on sums.
(\/) :: ( sig1           m a -> b)
     -> (          sig2  m a -> b)
     -> ((sig1 :+: sig2) m a -> b)
(alg1 \/ _   ) (L op) = alg1 op
(_    \/ alg2) (R op) = alg2 op

infixr 4 \/

data NonDet m k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance Effect NonDet where
  hfmap _ Empty      = Empty
  hfmap _ (Choose k) = Choose k

  handle _ Empty      = Empty
  handle _ (Choose k) = Choose k

instance Subset NonDet sig => Alternative (Eff sig) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))


newtype Fail m k = Fail String
  deriving (Functor)

instance Effect Fail where
  hfmap _ (Fail s) = Fail s

  handle _ (Fail s) = Fail s

instance Subset Fail sig => MonadFail (Eff sig) where
  fail = send . Fail


class (Effect sub, Effect sup) => Subset sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Effect sub => Subset sub sub where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} (Effect sub, Effect sup) => Subset sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} (Effect sub', Subset sub sup) => Subset sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


instance Effect sig => Functor (Eff sig) where
  fmap = liftM

instance Effect sig => Applicative (Eff sig) where
  pure = Return
  (<*>) = ap

instance Effect sig => Monad (Eff sig) where
  m >>= k = fold k Eff m
