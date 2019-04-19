{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Reader
( Reader(..)
, ask
, asks
, local
, runReader
, ReaderC(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
runReader r c = runReaderC c r
{-# INLINE runReader #-}

newtype ReaderC r m a = ReaderC { runReaderC :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderC r m) where
  pure = ReaderC . const . pure
  {-# INLINE pure #-}
  ReaderC f <*> ReaderC a = ReaderC (liftA2 (<*>) f a)
  {-# INLINE (<*>) #-}
  ReaderC u *> ReaderC v = ReaderC $ \ r -> u r *> v r
  {-# INLINE (*>) #-}
  ReaderC u <* ReaderC v = ReaderC $ \ r -> u r <* v r
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (ReaderC r m) where
  empty = ReaderC (const empty)
  {-# INLINE empty #-}
  ReaderC l <|> ReaderC r = ReaderC (liftA2 (<|>) l r)
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= runReader r . f)
  {-# INLINE (>>=) #-}

instance MonadFail m => MonadFail (ReaderC r m) where
  fail = ReaderC . const . fail
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (ReaderC r m) where
  liftIO = ReaderC . const . liftIO
  {-# INLINE liftIO #-}

instance (Alternative m, Monad m) => MonadPlus (ReaderC r m)

instance MonadTrans (ReaderC r) where
  lift = ReaderC . const
  {-# INLINE lift #-}

instance Carrier sig m => Carrier (Reader r :+: sig) (ReaderC r m) where
  eff (L (Ask       k)) = ReaderC (\ r -> runReader r (k r))
  eff (L (Local f m k)) = ReaderC (\ r -> runReader (f r) m) >>= k
  eff (R other)         = ReaderC (\ r -> eff (handlePure (runReader r) other))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
