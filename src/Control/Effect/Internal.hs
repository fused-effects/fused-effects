{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PolyKinds, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, runEff
, send
, Subset(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Handler
import Control.Effect.Fail.Internal
import Control.Effect.Lift.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum.Internal
import Control.Monad (liftM, ap)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Prelude hiding (fail)

newtype Eff h a = Eff { unEff :: forall x . (a -> h x) -> h x }

runEff :: (a -> f x) -> Eff f a -> f x
runEff = flip unEff
{-# INLINE runEff #-}

instance Functor (Eff h) where
  fmap = liftM

instance Applicative (Eff h) where
  pure a = Eff ($ a)

  (<*>) = ap

instance Monad (Eff h) where
  return = pure

  Eff m >>= f = Eff (\ k -> m (runEff k . f))


-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Subset effect sig, TermAlgebra m sig) => effect m (m a) -> m a
send = con . inj

instance TermAlgebra h sig => TermAlgebra (Eff h) sig where
  var = pure
  con op = Eff (\ k -> con (hfmap (runEff var) (fmap' (runEff k) op)))

instance TermAlgebra h sig => TermMonad (Eff h) sig


instance (Subset (Lift IO) sig, TermAlgebra m sig) => MonadIO (Eff m) where
  liftIO = send . Lift . fmap pure


instance (Subset NonDet sig, TermAlgebra m sig) => Alternative (Eff m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))


instance (Subset Fail sig, TermAlgebra m sig) => MonadFail (Eff m) where
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
