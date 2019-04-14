{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Effect.Trace
( Trace(..)
, trace
, runTraceByPrinting
, TraceByPrintingC(..)
, runTraceByIgnoring
, TraceByIgnoringC(..)
, runTraceByReturning
, TraceByReturningC(..)
) where

import Control.Applicative (Alternative(..))
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import Control.Monad.Base
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.Bifunctor (first)
import Data.Coerce
import System.IO

data Trace (m :: * -> *) k = Trace
  { traceMessage :: String
  , traceCont    :: k
  }
  deriving (Functor)

instance HFunctor Trace where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Trace where
  handle state handler (Trace s k) = Trace s (handler (k <$ state))

-- | Append a message to the trace log.
trace :: (Member Trace sig, Carrier sig m) => String -> m ()
trace message = send (Trace message (pure ()))


-- | Run a 'Trace' effect, printing traces to 'stderr'.
runTraceByPrinting :: TraceByPrintingC m a -> m a
runTraceByPrinting = runTraceByPrintingC

newtype TraceByPrintingC m a = TraceByPrintingC { runTraceByPrintingC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans TraceByPrintingC where
  lift = TraceByPrintingC
  {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (TraceByPrintingC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl TraceByPrintingC where
  type StT TraceByPrintingC a = a
  liftWith f = TraceByPrintingC $ f runTraceByPrinting
  restoreT   = TraceByPrintingC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceByPrintingC m) where
  type StM (TraceByPrintingC m) a = ComposeSt TraceByPrintingC m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadIO m, Carrier sig m) => Carrier (Trace :+: sig) (TraceByPrintingC m) where
  eff (L (Trace s k)) = liftIO (hPutStrLn stderr s) *> k
  eff (R other)       = TraceByPrintingC (eff (handleCoercible other))
  {-# INLINE eff #-}


-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runTraceByIgnoring (trace a *> pure b)) == b
runTraceByIgnoring :: TraceByIgnoringC m a -> m a
runTraceByIgnoring = runTraceByIgnoringC

newtype TraceByIgnoringC m a = TraceByIgnoringC { runTraceByIgnoringC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO, MonadPlus)

instance MonadTrans TraceByIgnoringC where
  lift = TraceByIgnoringC
  {-# INLINE lift #-}

instance MonadBase b m => MonadBase b (TraceByIgnoringC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl TraceByIgnoringC where
  type StT TraceByIgnoringC a = a
  liftWith f = TraceByIgnoringC $ f runTraceByIgnoring
  restoreT   = TraceByIgnoringC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceByIgnoringC m) where
  type StM (TraceByIgnoringC m) a = ComposeSt TraceByIgnoringC m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance Carrier sig m => Carrier (Trace :+: sig) (TraceByIgnoringC m) where
  eff (L trace) = traceCont trace
  eff (R other) = TraceByIgnoringC (eff (handleCoercible other))
  {-# INLINE eff #-}


-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTraceByReturning (trace a *> trace b *> pure c)) == ([a, b], c)
runTraceByReturning :: Functor m => TraceByReturningC m a -> m ([String], a)
runTraceByReturning = fmap (first reverse) . runState [] . runTraceByReturningC

newtype TraceByReturningC m a = TraceByReturningC { runTraceByReturningC :: StateC [String] m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadFail, MonadIO, MonadPlus, MonadTrans, MonadTransControl)

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (TraceByReturningC m) where
  eff (L (Trace m k)) = TraceByReturningC (modify (m :)) *> k
  eff (R other)       = TraceByReturningC (eff (R (handleCoercible other)))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Pure
