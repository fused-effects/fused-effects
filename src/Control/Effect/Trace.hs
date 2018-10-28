{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
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

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.IO.Class
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
trace message = send (Trace message (ret ()))


-- | Run a 'Trace' effect, printing traces to 'stderr'.
runTraceByPrinting :: (MonadIO m, Carrier sig m) => Eff (TraceByPrintingC m) a -> m a
runTraceByPrinting = runTraceByPrintingC . interpret

newtype TraceByPrintingC m a = TraceByPrintingC { runTraceByPrintingC :: m a }

instance (MonadIO m, Carrier sig m) => Carrier (Trace :+: sig) (TraceByPrintingC m) where
  ret = TraceByPrintingC . ret
  eff = TraceByPrintingC . (alg \/ eff . handlePure runTraceByPrintingC)
    where alg (Trace s k) = liftIO (hPutStrLn stderr s) *> runTraceByPrintingC k


-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runTraceByIgnoring (trace a *> pure b)) == b
runTraceByIgnoring :: Carrier sig m => Eff (TraceByIgnoringC m) a -> m a
runTraceByIgnoring = runTraceByIgnoringC . interpret

newtype TraceByIgnoringC m a = TraceByIgnoringC { runTraceByIgnoringC :: m a }

instance Carrier sig m => Carrier (Trace :+: sig) (TraceByIgnoringC m) where
  ret = TraceByIgnoringC . ret
  eff = handleSum (TraceByIgnoringC . eff . handlePure runTraceByIgnoringC) (\ (Trace _ k) -> k)


-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runTraceByReturning (trace a *> trace b *> pure c)) == ([a, b], c)
runTraceByReturning :: (Carrier sig m, Effect sig, Functor m) => Eff (TraceByReturningC m) a -> m ([String], a)
runTraceByReturning = fmap (first reverse) . flip runTraceByReturningC [] . interpret

newtype TraceByReturningC m a = TraceByReturningC { runTraceByReturningC :: [String] -> m ([String], a) }

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (TraceByReturningC m) where
  ret a = TraceByReturningC (\ s -> ret (s, a))
  eff op = TraceByReturningC (\ s -> (alg s \/ eff . handleState s runTraceByReturningC) op)
    where alg s (Trace m k) = runTraceByReturningC k (m : s)


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
