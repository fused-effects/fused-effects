{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Control.Effect
( Eff
, send
, fold
, foldA
, liftAlg
, relay
, Effect(..)
, Carrier(..)
, Void
, run
, (:+:)(..)
, Subset(..)
, Lift(..)
, runM
, NonDet(..)
, Fail(..)
, Cut(..)
, cutfail
, call
, cut
, Symbol(..)
, satisfy
, char
, digit
, expr
, term
, factor
-- , parse
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.Split
import Control.Monad (ap, join, liftM)
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

liftAlg :: (Effect sig, Carrier c f, Monad (c (Eff sig)))
        => (forall a .  eff          (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
        -> (forall a . (eff :+: sig) (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
liftAlg alg1 = alg1 \/ alg2
  where alg2 op = suspend >>= \ state -> joinl (Eff (fmap' pure (handle state op)))

relay :: (Effect eff, Effect sig, Carrier c f, Monad (c (Eff sig)))
      => (forall a . eff (c (Eff sig)) (c (Eff sig) a) -> c (Eff sig) a)
      -> (forall a . Eff (eff :+: sig) a -> c (Eff sig) a)
relay alg = foldA (liftAlg alg)
{-# INLINE relay #-}


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

runM :: Monad m => Eff (Lift m) a -> m a
runM = foldA (join . unLift)


data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Functor, Ord, Show)

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


data Cut m k
  = Cut
  | forall b . Call (m b) (b -> k)

deriving instance Functor (Cut m)

instance Effect Cut where
  hfmap _ Cut        = Cut
  hfmap f (Call m k) = Call (f m) k

  handle _     Cut        = Cut
  handle state (Call m k) = Call (resume (m <$ state)) (wrap . resume . fmap k)

cutfail :: Subset Cut sig => Eff sig a
cutfail = send Cut

call :: Subset Cut sig => Eff sig a -> Eff sig a
call m = send (Call m pure)

cut :: (Subset NonDet sig, Subset Cut sig) => Eff sig ()
cut = skip <|> cutfail

skip :: Applicative m => m ()
skip = pure ()

-- runCut :: Subset NonDet sig => Eff (Cut :+: sig) a -> Eff sig a
-- runCut = go empty
--   where go :: Subset NonDet sig => Eff sig a -> Eff (Cut :+: sig) a -> Eff sig a
--         go q (Return a) = pure a <|> q
--         go q Empty      = q
--         go _ Cut        = empty
--         go q (Choose k) = go (go q (k False)) (k True)
--         go q (Call m k) = go empty m >>= go q . k
--         go q (Other op) = Eff (hfmap (go empty) op) <|> q


data Symbol m k
  = Symbol (Char -> Bool) (Char -> k)
  deriving (Functor)

instance Effect Symbol where
  hfmap _ (Symbol sat k) = Symbol sat k

  handle _ (Symbol sat k) = Symbol sat k

satisfy :: Subset Symbol sig => (Char -> Bool) -> Eff sig Char
satisfy sat = send (Symbol sat pure)

char :: Subset Symbol sig => Char -> Eff sig Char
char c = satisfy (== c)

digit :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Char
digit = foldr ((<|>) . char) empty ['0'..'9']

expr :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr) <|> pure i

term :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term) <|> pure i

factor :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
factor = read <$> some digit
     <|> char '(' *> expr <* char ')'

-- parse :: Subset NonDet sig => String -> Eff (Symbol :+: sig) a -> Eff sig a
-- parse input = fmap snd . flip runStateH input . relay alg
--   where alg (Symbol p k) = StateH (\ s -> case s of
--           c:cs | p c -> runStateH (k c) cs
--           _          -> empty)


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
