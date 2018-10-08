{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleInstances, MultiParamTypeClasses, PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Control.Effect
( Eff
, send
, fold
, foldA
, interpret
, interpret2
, interpretRest
, reinterpret
, reinterpret2
, reinterpret_2
, reinterpret2_2
, reinterpretRest
, Effect(..)
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
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) (Eff effects a))

class Effect sig where
  fmap' :: (a -> b) -> (sig m a -> sig m b)
  default fmap' :: Functor (sig m) => (a -> b) -> (sig m a -> sig m b)
  fmap' = fmap

  hfmap :: (forall x . m x -> n x) -> sig m a -> sig n a

  handle :: (Carrier c f, Monad n)
         => f ()
         -> sig (c n) (c n a)
         -> sig n (c n a)


send :: Subset effect sig => effect (Eff sig) (Eff sig a) -> Eff sig a
send = Eff . inj


fold :: Effect sig
     => (a -> b)
     -> (sig (Eff sig) b -> b)
     -> (Eff sig a -> b)
fold gen alg = go
  where go (Return x) = gen x
        go (Eff op)   = alg (fmap' go op)

foldA :: forall sig f
      .  (Effect sig, Applicative f)
      => (forall a . sig f (f a) -> f a)
      -> (forall a . Eff sig a -> f a)
foldA alg = go
  where go :: Eff sig a -> f a
        go (Return x) = pure x
        go (Eff op)   = alg (hfmap go (fmap' go op))

-- | Interpret an 'Effect'’s requests into a 'Carrier' using the passed algebra.
interpret :: (Effect eff, Effect sig, Carrier c f, Monad (c (Eff sig)))
          => (forall a . eff (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
          -> (forall a . Eff (eff :+: sig) a -> c (Eff sig) a)
interpret alg = foldA (alg \/ interpretRest)
{-# INLINE interpret #-}

interpret2 :: (Effect eff1, Effect eff2, Effect sig, Carrier c f, Monad (c (Eff sig)))
           => (forall a . eff1 (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
           -> (forall a . eff2 (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
           -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c (Eff sig) a)
interpret2 alg1 alg2 = foldA (alg1 \/ alg2 \/ interpretRest)
{-# INLINE interpret2 #-}

interpretRest :: (Effect sig, Carrier c f, Monad (c (Eff sig)))
              => sig (c (Eff sig)) (c (Eff sig) a)
              -> c (Eff sig) a
interpretRest op = suspend >>= \ state -> joinl (Eff (fmap' pure (handle state op)))


reinterpret :: (Effect eff, Effect sig, Effect new, Carrier c f, Monad (c (Eff (new :+: sig))))
            => (forall a . eff (c (Eff (new :+: sig))) (c (Eff (new :+: sig)) a) -> c (Eff (new :+: sig)) a)
            -> (forall a . Eff (eff :+: sig) a -> c (Eff (new :+: sig)) a)
reinterpret alg = foldA (alg \/ reinterpretRest)
{-# INLINE reinterpret #-}

reinterpret2 :: (Effect eff1, Effect eff2, Effect sig, Effect new, Carrier c f, Monad (c (Eff (new :+: sig))))
             => (forall a . eff1 (c (Eff (new :+: sig))) (c (Eff (new :+: sig)) a) -> c (Eff (new :+: sig)) a)
             -> (forall a . eff2 (c (Eff (new :+: sig))) (c (Eff (new :+: sig)) a) -> c (Eff (new :+: sig)) a)
             -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c (Eff (new :+: sig)) a)
reinterpret2 alg1 alg2 = foldA (alg1 \/ alg2 \/ reinterpretRest)
{-# INLINE reinterpret2 #-}

reinterpret_2 :: (Effect eff, Effect sig, Effect new1, Effect new2, Carrier c f, Monad (c (Eff (new1 :+: new2 :+: sig))))
             => (forall a . eff (c (Eff (new1 :+: new2 :+: sig))) (c (Eff (new1 :+: new2 :+: sig)) a) -> c (Eff (new1 :+: new2 :+: sig)) a)
             -> (forall a . Eff (eff :+: sig) a -> c (Eff (new1 :+: new2 :+: sig)) a)
reinterpret_2 alg = foldA (alg \/ reinterpretRest)
{-# INLINE reinterpret_2 #-}

reinterpret2_2 :: (Effect eff1, Effect eff2, Effect sig, Effect new1, Effect new2, Carrier c f, Monad (c (Eff (new1 :+: new2 :+: sig))))
             => (forall a . eff1 (c (Eff (new1 :+: new2 :+: sig))) (c (Eff (new1 :+: new2 :+: sig)) a) -> c (Eff (new1 :+: new2 :+: sig)) a)
             -> (forall a . eff2 (c (Eff (new1 :+: new2 :+: sig))) (c (Eff (new1 :+: new2 :+: sig)) a) -> c (Eff (new1 :+: new2 :+: sig)) a)
             -> (forall a . Eff (eff1 :+: eff2 :+: sig) a -> c (Eff (new1 :+: new2 :+: sig)) a)
reinterpret2_2 alg1 alg2 = foldA (alg1 \/ alg2 \/ reinterpretRest)
{-# INLINE reinterpret2_2 #-}

reinterpretRest :: (Effect sig, Effect new, Carrier c f, Monad (c (Eff (new :+: sig))))
                => sig (c (Eff (new :+: sig))) (c (Eff (new :+: sig)) a)
                -> c (Eff (new :+: sig)) a
reinterpretRest op = suspend >>= \ state -> joinl (Eff (fmap' pure (R (handle state op))))


data Void m k
  deriving (Functor)

instance Effect Void where
  hfmap _ v = case v of {}
  handle _ v = case v of {}

run :: Eff Void a -> a
run = fold id (\ v -> case v of {})


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

infixl 4 :+:

instance (Effect l, Effect r) => Effect (l :+: r) where
  hfmap f (L l) = L (hfmap f l)
  hfmap f (R r) = R (hfmap f r)

  fmap' f (L l) = L (fmap' f l)
  fmap' f (R r) = R (fmap' f r)

  handle state (L l) = L (handle state l)
  handle state (R r) = R (handle state r)

(\/) :: ( sig1           m a -> b)
     -> (          sig2  m a -> b)
     -> ((sig1 :+: sig2) m a -> b)
(alg1 \/ _   ) (L op) = alg1 op
(_    \/ alg2) (R op) = alg2 op

infixl 4 \/

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
