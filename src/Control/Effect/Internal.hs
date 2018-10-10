{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, runEff
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Handler
import Control.Effect.Fail.Internal
import Control.Effect.Lift.Internal
import Control.Effect.NonDet.Internal
import Control.Effect.Sum
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

instance (Subset NonDet sig, TermAlgebra m sig) => Alternative (Eff m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance Monad (Eff h) where
  return = pure

  Eff m >>= f = Eff (\ k -> m (runEff k . f))

instance (Subset Fail sig, TermAlgebra m sig) => MonadFail (Eff m) where
  fail = send . Fail

instance (Subset (Lift IO) sig, TermAlgebra m sig) => MonadIO (Eff m) where
  liftIO = send . Lift . fmap pure


instance TermAlgebra h sig => TermAlgebra (Eff h) sig where
  var = pure
  con op = Eff (\ k -> con (hfmap (runEff var) (fmap' (runEff k) op)))

instance TermAlgebra h sig => TermMonad (Eff h) sig
