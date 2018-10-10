{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Control.Effect.Internal
( Eff(..)
, runEff
, interpret
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

interpret :: Carrier sig carrier => Eff carrier a -> carrier a
interpret = runEff gen
{-# INLINE interpret #-}

instance Functor (Eff h) where
  fmap = liftM

instance Applicative (Eff h) where
  pure a = Eff ($ a)

  (<*>) = ap

instance (Subset NonDet sig, Carrier sig m) => Alternative (Eff m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance Monad (Eff h) where
  return = pure

  Eff m >>= f = Eff (\ k -> m (runEff k . f))

instance (Subset Fail sig, Carrier sig m) => MonadFail (Eff m) where
  fail = send . Fail

instance (Subset (Lift IO) sig, Carrier sig m) => MonadIO (Eff m) where
  liftIO = send . Lift . fmap pure


instance Carrier sig h => Carrier sig (Eff h) where
  gen = pure
  alg op = Eff (\ k -> alg (hfmap (runEff gen) (fmap' (runEff k) op)))

instance (Carrier sig h, Effect sig) => Effectful sig (Eff h)
