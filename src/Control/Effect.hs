{-# LANGUAGE EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, PolyKinds, TypeOperators, ViewPatterns #-}
module Control.Effect where

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) (Eff effects a))

data Void m a

run :: Eff Void a -> a
run (Return a) = a
run (Eff v) = case v of {}


data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)

data NonDet m a
  = Empty
  | Choose (m a) (m a)
  deriving (Eq, Ord, Show)

data Reader r m a
  = Ask' (r -> m a)
  | forall b . Local' (r -> r) (m b) (b -> m a)

data State s m a
  = Get' (s -> m a)
  | Put' s (m a)

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
