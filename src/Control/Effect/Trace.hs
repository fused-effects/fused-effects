{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Trace
( Trace(..)
, trace
, runPrintingTrace
, PrintingH(..)
, runIgnoringTrace
, IgnoringH(..)
, runReturningTrace
, ReturningH(..)
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import System.IO

data Trace m k = Trace String k
  deriving (Functor)

instance HFunctor Trace where
  hfmap _ (Trace s k) = Trace s k

instance Effect Trace where
  handle state handler (Trace s k) = Trace s (handler (k <$ state))

trace :: (Member Trace sig, Carrier sig m) => String -> m ()
trace message = send (Trace message (gen ()))


runPrintingTrace :: (MonadIO m, Carrier sig m) => Eff (PrintingH m) a -> m a
runPrintingTrace = runPrintingH . interpret

newtype PrintingH m a = PrintingH { runPrintingH :: m a }

instance (MonadIO m, Carrier sig m) => Carrier (Trace :+: sig) (PrintingH m) where
  gen = PrintingH . gen
  alg = algT \/ (PrintingH . alg . handlePure runPrintingH)
    where algT (Trace s k) = PrintingH (liftIO (hPutStrLn stderr s) *> runPrintingH k)


-- | Run a 'Trace' effect, ignoring all traces.
--
--   prop> run (runIgnoringTrace (trace a *> pure b)) == b
runIgnoringTrace :: Carrier sig m => Eff (IgnoringH m) a -> m a
runIgnoringTrace = runIgnoringH . interpret

newtype IgnoringH m a = IgnoringH { runIgnoringH :: m a }

instance Carrier sig m => Carrier (Trace :+: sig) (IgnoringH m) where
  gen = IgnoringH . gen
  alg = algT \/ (IgnoringH . alg . handlePure runIgnoringH)
    where algT (Trace _ k) = k


-- | Run a 'Trace' effect, returning all traces as a list.
--
--   prop> run (runReturningTrace (trace a *> trace b *> pure c)) == ([a, b], c)
runReturningTrace :: Effectful sig m => Eff (ReturningH m) a -> m ([String], a)
runReturningTrace = fmap (first reverse) . flip runReturningH [] . interpret

newtype ReturningH m a = ReturningH { runReturningH :: [String] -> m ([String], a) }

instance (Carrier sig m, Effect sig) => Carrier (Trace :+: sig) (ReturningH m) where
  gen a = ReturningH (\ s -> gen (s, a))
  alg = algT \/ algOther
    where algT (Trace m k) = ReturningH (runReturningH k . (m :))
          algOther op = ReturningH (\ s -> alg (handle (s, ()) (uncurry (flip runReturningH)) op))


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
