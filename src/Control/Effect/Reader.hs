{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader
( Reader(..)
, ask
, asks
, local
, runReader
, ReaderC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as MT
import Prelude hiding (fail)

data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance HFunctor (Reader r) where
  hmap _ (Ask k)       = Ask k
  hmap f (Local g m k) = Local g (f m) k

instance Effect (Reader r) where
  handle state handler (Ask k)       = Ask (handler . (<$ state) . k)
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (handler . fmap k)

-- | Retrieve the environment value.
--
--   prop> run (runReader a ask) == a
ask :: (Member (Reader r) sig, Carrier sig m) => m r
ask = send (Ask pure)

-- | Project a function out of the current environment value.
--
--   prop> snd (run (runReader a (asks (applyFun f)))) == applyFun f a
asks :: (Member (Reader r) sig, Carrier sig m) => (r -> a) -> m a
asks f = send (Ask (pure . f))

-- | Run a computation with an environment value locally modified by the passed function.
--
--   prop> run (runReader a (local (applyFun f) ask)) == applyFun f a
--   prop> run (runReader a ((,,) <$> ask <*> local (applyFun f) ask <*> ask)) == (a, applyFun f a, a)
local :: (Member (Reader r) sig, Carrier sig m) => (r -> r) -> m a -> m a
local f m = send (Local f m pure)


-- | Run a 'Reader' effect with the passed environment value.
--
--   prop> run (runReader a (pure b)) == b
runReader :: r -> ReaderC r m a -> m a
runReader r = flip MT.runReaderT r . runReaderC

newtype ReaderC r m a = ReaderC { runReaderC :: MT.ReaderT r m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance Carrier sig m => Carrier (Reader r :+: sig) (ReaderC r m) where
  eff (L (Ask       k)) = ReaderC MT.ask >>= k
  eff (L (Local f m k)) = ReaderC (MT.local f (runReaderC m)) >>= k
  eff (R other)         = ReaderC (MT.ReaderT (\ r -> eff (handlePure (runReader r) other)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
