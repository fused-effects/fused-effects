{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Trace
( Trace(..)
, trace
, runPrintingTrace
, runIgnoringTrace
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Sum
import Control.Monad.IO.Class
import System.IO

data Trace m k = Trace String k
  deriving (Functor)

instance HFunctor Trace where
  hfmap _ (Trace s k) = Trace s k

instance Effect Trace where
  handle state handler (Trace s k) = Trace s (handler (k <$ state))

trace :: (Subset Trace sig, Effectful sig m) => String -> m ()
trace message = send (Trace message (pure ()))


runPrintingTrace :: (MonadIO m, Carrier sig m) => Eff (PrintingH m) a -> m a
runPrintingTrace = runPrintingH . interpret

newtype PrintingH m a = PrintingH { runPrintingH :: m a }

instance (MonadIO m, Carrier sig m) => Carrier (Trace :+: sig) (PrintingH m) where
  gen = PrintingH . gen
  alg = algT \/ (PrintingH . alg . handlePure runPrintingH)
    where algT (Trace s k) = PrintingH (liftIO (hPutStrLn stderr s) *> runPrintingH k)


runIgnoringTrace :: Carrier sig m => Eff (IgnoringH m) a -> m a
runIgnoringTrace = runIgnoringH . interpret

newtype IgnoringH m a = IgnoringH { runIgnoringH :: m a }

instance Carrier sig m => Carrier (Trace :+: sig) (IgnoringH m) where
  gen = IgnoringH . gen
  alg = algT \/ (IgnoringH . alg . handlePure runIgnoringH)
    where algT (Trace _ k) = k
