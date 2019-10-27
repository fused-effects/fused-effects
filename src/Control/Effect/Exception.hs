{-# LANGUAGE DeriveFunctor, ExistentialQuantification, RankNTypes, StandaloneDeriving #-}
-- | Operations from "Control.Exception" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- @since 1.0.0.0
module Control.Effect.Exception
( -- * Lifted "Control.Exception" operations
  Exc.Exception(..)
, Exc.SomeException(..)
, throwIO
, catch
, catches
, Handler(..)
, catchJust
, handle
, handleJust
, try
, tryJust
, mask
, interruptible
, allowInterrupt
, bracket
, bracket_
, bracketOnError
, finally
, onException
  -- * Lift effect
, Lift(..)
, sendM
, liftWith
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Effect.Lift
import qualified Control.Exception as Exc

-- | See @"Control.Exception".'Exc.throwIO'@.
--
-- @since 1.0.0.0
throwIO :: (Exc.Exception e, Has (Lift IO) sig m) => e -> m a
throwIO = sendM . Exc.throwIO

-- | See @"Control.Exception".'Exc.catch'@.
--
-- @since 1.0.0.0
catch :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
catch m h = liftWith $ \ ctx run -> run (m <$ ctx) `Exc.catch` (run . (<$ ctx) . h)

-- | See @"Control.Exception".'Exc.catches'@.
--
-- @since 1.0.0.0
catches :: Has (Lift IO) sig m => m a -> [Handler m a] -> m a
catches m hs = liftWith $ \ ctx run ->
  Exc.catches (run (m <$ ctx)) (map (\ (Handler h) -> Exc.Handler (run . (<$ ctx) . h)) hs)

-- | See @"Control.Exception".'Exc.Handler'@.
--
-- @since 1.0.0.0
data Handler m a
  = forall e . Exc.Exception e => Handler (e -> m a)

deriving instance Functor m => Functor (Handler m)

-- | See @"Control.Exception".'Exc.catchJust'@.
--
-- @since 1.0.0.0
catchJust
  :: (Exc.Exception e, Has (Lift IO) sig m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust p m h = liftWith $ \ ctx run -> Exc.catchJust p (run (m <$ ctx)) (run . (<$ ctx) . h)

-- | See @"Control.Exception".'Exc.handle'@.
--
-- @since 1.0.0.0
handle :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> m a) -> m a -> m a
handle = flip catch

-- | See @"Control.Exception".'Exc.handleJust'@.
--
-- @since 1.0.0.0
handleJust
  :: (Exc.Exception e, Has (Lift IO) sig m)
  => (e -> Maybe b)
  -> (b -> m a)
  -> m a
  -> m a
handleJust p = flip (catchJust p)

-- | See @"Control.Exception".'Exc.try'@.
--
-- @since 1.0.0.0
try :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> m (Either e a)
try m = (Right <$> m) `catch` (pure . Left)

-- | See @"Control.Exception".'Exc.tryJust'@.
--
-- @since 1.0.0.0
tryJust :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p m = catchJust p (Right <$> m) (pure . Left)

-- | See @"Control.Exception".'Exc.mask'@.
--
-- @since 1.0.0.0
mask :: Has (Lift IO) sig m => ((forall a . m a -> m a) -> m b) -> m b
mask with = liftWith $ \ ctx run -> Exc.mask $ \ restore ->
  run (with (\ m -> liftWith $ \ ctx' run' -> restore (run' (m <$ ctx'))) <$ ctx)

-- | See @"Control.Exception".'Exc.allowInterrupt'@.
--
-- @since 1.0.0.0
allowInterrupt :: Has (Lift IO) sig m => m ()
allowInterrupt = sendM Exc.allowInterrupt

-- | See @"Control.Exception".'Exc.interruptible'@.
--
-- @since 1.0.0.0
interruptible :: Has (Lift IO) sig m => m a -> m a
interruptible m = liftWith $ \ ctx run -> Exc.interruptible (run (m <$ ctx))

-- | See @"Control.Exception".'Exc.bracket'@.
--
-- @since 1.0.0.0
bracket
  :: Has (Lift IO) sig m
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracket acquire release m = mask $ \ restore -> do
  a <- acquire
  r <- restore (m a) `onException` release a
  r <$ release a

-- | See @"Control.Exception".'Exc.bracket_'@.
--
-- @since 1.0.0.0
bracket_
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m c
  -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | See @"Control.Exception".'Exc.bracketOnError'@.
--
-- @since 1.0.0.0
bracketOnError
  :: Has (Lift IO) sig m
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracketOnError acquire release m = mask $ \ restore -> do
  a <- acquire
  restore (m a) `onException` release a

-- | See @"Control.Exception".'Exc.finally'@.
--
-- @since 1.0.0.0
finally
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m a
finally m sequel = mask $ \ restore -> (restore m `onException` sequel) <* sequel

-- | See @"Control.Exception".'Exc.onException'@.
--
-- @since 1.0.0.0
onException :: Has (Lift IO) sig m => m a -> m b -> m a
onException io what = io `catch` \e -> what >> throwIO (e :: Exc.SomeException)
