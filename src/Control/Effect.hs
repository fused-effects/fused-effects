{-# LANGUAGE EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, PolyKinds, RankNTypes, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Control.Effect where

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) a)


type f ~> g = forall x . f x -> g x

class HFunctor sig where
  hmap :: (Functor f, Functor g) => (f ~> g) -> (sig f ~> sig g)


class HFunctor sig => Effect sig where
  emap :: Monad m => (m a -> m b) -> (sig m a -> sig m b)

  handle :: (Monad m, Monad n, Functor c) => c () -> (forall x . c (m x) -> n (c x)) -> (sig m a -> sig n (c a))


data Void m a

run :: Eff Void a -> a
run (Return a) = a
run (Eff v) = case v of {}


data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)


pattern Other :: r (Eff (l :+: r)) a -> Eff (l :+: r) a
pattern Other s = Eff (R s)


data NonDet m a
  = Empty'
  | Choose' (Bool -> m a)

pattern Empty :: Subset NonDet effects => Eff effects a
pattern Empty <- Eff (prj -> Just Empty')

pattern Choose :: Subset NonDet effects => (Bool -> Eff effects a) -> Eff effects a
pattern Choose k <- Eff (prj -> Just (Choose' k))


data Reader r m a
  = Ask' (r -> m a)
  | forall b . Local' (r -> r) (m b) (b -> m a)

pattern Ask :: Subset (Reader r) effects => (r -> Eff effects a) -> Eff effects r
pattern Ask k <- Eff (prj -> Just (Ask' k))

pattern Local :: Subset (Reader r) effects => (r -> r) -> Eff effects b -> (b -> Eff effects a) -> Eff effects a
pattern Local f m k <- Eff (prj -> Just (Local' f m k))


data State s m a
  = Get' (s -> m a)
  | Put' s (m a)

pattern Get :: Subset (State s) effects => (s -> Eff effects a) -> Eff effects s
pattern Get k <- Eff (prj -> Just (Get' k))

pattern Put :: Subset (State s) effects => s -> Eff effects a -> Eff effects ()
pattern Put s k <- Eff (prj -> Just (Put' s k))


data Fail m a = Fail' String

pattern Fail :: Subset Fail effects => String -> Eff effects a
pattern Fail s <- Eff (prj -> Just (Fail' s))


class Subset sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Subset sub sub where
  inj = id
  prj = Just

instance Subset sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance Subset sub sup => Subset sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


instance Functor (effects (Eff effects)) => Functor (Eff effects) where
  fmap f (Return a) = Return (f a)
  fmap f (Eff sub)  = Eff (fmap f sub)
