{-# LANGUAGE DeriveFunctor, DeriveGeneric, ExistentialQuantification, KindSignatures, StandaloneDeriving, TypeOperators #-}
module Control.Effect.Internal
( -- * Effects
  Empty(..)
, Error
, Lift(..)
, NonDet
, Reader(..)
, State(..)
, Throw(..)
, Writer(..)
) where

import Control.Effect.Catch.Internal
import Control.Effect.Choose.Internal
import Control.Effect.Class
import Control.Effect.Sum ((:+:))
import GHC.Generics (Generic1)

-- | @since 1.0.0.0
data Empty (m :: * -> *) k = Empty
  deriving (Functor, Generic1)

instance HFunctor Empty
instance Effect   Empty


-- | @since 0.1.0.0
type Error e = Throw e :+: Catch e


-- | @since 0.1.0.0
newtype Lift sig m k = Lift { unLift :: sig (m k) }
  deriving (Functor, Generic1)

instance Functor m => HFunctor (Lift m)
instance Functor m => Effect   (Lift m)


-- | The nondeterminism effect is the composition of 'Empty' and 'Choose' effects.
--
-- @since 0.1.0.0
type NonDet = Empty :+: Choose


-- | @since 0.1.0.0
data Reader r m k
  = Ask (r -> m k)
  | forall b . Local (r -> r) (m b) (b -> m k)

deriving instance Functor m => Functor (Reader r m)

instance HFunctor (Reader r) where
  hmap f (Ask k)       = Ask           (f . k)
  hmap f (Local g m k) = Local g (f m) (f . k)

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)


-- | @since 0.1.0.0
data State s m k
  = Get (s -> m k)
  | Put s (m k)
  deriving (Functor, Generic1)

instance HFunctor (State s)
instance Effect   (State s)


-- | @since 1.0.0.0
data Throw e (m :: * -> *) k
  = Throw e
  deriving (Functor, Generic1)

instance HFunctor (Throw e)
instance Effect   (Throw e)


-- | @since 0.1.0.0
data Writer w m k
  = Tell w (m k)
  | forall a . Listen (m a) (w -> a -> m k)
  | forall a . Censor (w -> w) (m a) (a -> m k)

deriving instance Functor m => Functor (Writer w m)

instance HFunctor (Writer w) where
  hmap f (Tell w     k) = Tell w         (f       k)
  hmap f (Listen   m k) = Listen   (f m) ((f .) . k)
  hmap f (Censor g m k) = Censor g (f m) (f     . k)
  {-# INLINE hmap #-}

instance Effect (Writer w) where
  handle state handler (Tell w     k) = Tell w                          (handler (k <$ state))
  handle state handler (Listen   m k) = Listen   (handler (m <$ state)) (fmap handler . fmap . k)
  handle state handler (Censor f m k) = Censor f (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}
