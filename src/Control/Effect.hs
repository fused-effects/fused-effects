{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Effect
( send
, interpretRest
, reinterpretRest
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
import Control.Monad.Codensity
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

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

instance TermAlgebra h sig => TermAlgebra (Codensity h) sig where
  var = pure
  con = algCod con

algCod :: TermAlgebra h sig
       => (forall a . sig h (h a) -> h a)
       -> (forall a . sig (Codensity h) (Codensity h a) -> Codensity h a)
algCod alg op = Codensity (\ k -> alg (hfmap (runCodensity var) (fmap' (runCodensity k) op)))


class (Monad m, TermAlgebra m sig) => TermMonad m sig | m -> sig

instance TermAlgebra h sig => TermMonad (Codensity h) sig


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Subset effect sig, TermAlgebra m sig) => effect m (m a) -> m a
send = con . inj


-- | Interpret any requests in higher-order positions in the remaining effects.
--
--   This is typically passed to 'foldH' as the last of a '\/'-chain of algebras, and can be used uniformly regardless of how many effects are being handled.
interpretRest :: (Carrier f c, TermMonad m sig)
              => sig (c m) (c m a)
              -> c m a
interpretRest op = suspend (\ state -> joinl (con (fmap' var (handle state op))))

-- | Reinterpret any requests in higher-order positions in the remaining effects.
--
--   This is typically passed to 'foldH' as the last of a '\/'-chain of algebras, and can be used uniformly regardless of how many effects are being handled.
reinterpretRest :: (Effect sig, Carrier f c, TermMonad m (new :+: sig))
                => sig (c m) (c m a)
                -> c m a
reinterpretRest op = suspend (\ state -> joinl (con (fmap' var (R (handle state op)))))

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

instance (Subset (Lift IO) sig, TermAlgebra m sig) => MonadIO (Codensity m) where
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

instance (Subset NonDet sig, TermAlgebra m sig) => Alternative (Codensity m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))


newtype Fail m k = Fail String
  deriving (Functor)

instance Effect Fail where
  hfmap _ (Fail s) = Fail s

  handle _ (Fail s) = Fail s

instance (Subset Fail sig, TermAlgebra m sig) => MonadFail (Codensity m) where
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
