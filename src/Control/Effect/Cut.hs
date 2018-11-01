{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
) where

import Control.Effect.Carrier
import Control.Effect.Sum

data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)

deriving instance Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) k

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)

cutfail :: (Carrier sig m, Member Cut sig) => m a
cutfail = send Cutfail

call :: (Carrier sig m, Member Cut sig) => m a -> m a
call m = send (Call m ret)
