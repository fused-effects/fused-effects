{-# LANGUAGE DeriveFunctor, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Control.Effect.Sum
( (:+:)(..)
, (\/)
, handleSum
, Member(..)
, send
) where

import Control.Effect.Carrier

data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Functor, Ord, Show)

infixr 4 :+:

instance (HFunctor l, HFunctor r) => HFunctor (l :+: r) where
  hmap f (L l) = L (hmap f l)
  hmap f (R r) = R (hmap f r)

  fmap' f (L l) = L (fmap' f l)
  fmap' f (R r) = R (fmap' f r)

instance (Effect l, Effect r) => Effect (l :+: r) where
  handle state handler (L l) = L (handle state handler l)
  handle state handler (R r) = R (handle state handler r)


-- | Lift algebras for either side of a sum into a single algebra on sums.
(\/) :: ( sig1           m a -> b)
     -> (          sig2  m a -> b)
     -> ((sig1 :+: sig2) m a -> b)
(alg1 \/ _   ) (L op) = alg1 op
(_    \/ alg2) (R op) = alg2 op

infixr 4 \/

handleSum :: (          sig2  m a -> b)
          -> ( sig1           m a -> b)
          -> ((sig1 :+: sig2) m a -> b)
handleSum alg1 _    (R op) = alg1 op
handleSum _    alg2 (L op) = alg2 op


class Member (sub :: (* -> *) -> (* -> *)) sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Member sub sub where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} Member sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} Member sub sup => Member sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member effect sig, Carrier sig m) => effect m (m a) -> m a
send = eff . inj
