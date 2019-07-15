-- This example shows how to reinterpret a simple, first-order "logging" effect,
-- in terms of itself, in order to change the type of the values it logs.
--
-- * First, we will define a structured log message type, which is the type our
--   application prefers to log in.
--
-- * Next, we will define a logging carrier that prints strings to stdout.
--
-- * Finally, we will bridge the two with an effect carrier that reinterprets
--   structured log messages as strings.


{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module ReinterpretLog
  ( spec
  ) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce            (coerce)
import Data.Function          ((&))
import Data.Kind              (Type)
import GHC.Generics           (Generic1)
import Prelude                hiding (log)
import Test.Hspec


--------------------------------------------------------------------------------
-- The application
--------------------------------------------------------------------------------

-- Our structured log message. In this example, we just tag a 'String' with its
-- severity, but this can be anything.
data Message
  = Debug String
  | Info String

-- Render a structured log message as a string.
renderLogMessage ::
     Message
  -> String
renderLogMessage = \case
  Debug message -> "[debug] " ++ message
  Info  message -> "[info] "  ++ message

-- The application: it logs two messages, then quits.
application ::
     ( Carrier sig m
     , Member (Log Message) sig
     )
  => m ()
application = do
  log (Debug "debug message")
  log (Info "info message")

-- The application runner. Interpret the application by:
--
-- * Reinterpreting 'Log Message' effects as 'Log String' effects.
-- * Interpreting 'Log String' effects by printing to stdout.
runApplication :: IO ()
runApplication =
  application
    -- Type inference is picking our concrete monad stack.
    --
    -- Here its type is:
    --
    --   ReinterpretLogC Message String (LogStdoutC (LiftC IO)) ()

    & reinterpretLog renderLogMessage
    -- Now its type is:
    --
    --   LogStdoutC (LiftC IO) ()

    & runLogStdout
    -- Now its type is:
    --
    --   LiftC IO ()

    & runM
    -- Now its type is:
    --
    --   IO ()


--------------------------------------------------------------------------------
-- The logging effect
--------------------------------------------------------------------------------

-- Log an 'a', then continue with 'k'.
data Log (a :: Type) (m :: Type -> Type) (k :: Type)
  = Log a (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

-- Log an 'a'.
log ::
     ( Carrier sig m
     , Member (Log a) sig
     )
  => a
  -> m ()
log x =
  send (Log x (pure ()))


--------------------------------------------------------------------------------
-- The logging effect carriers
--------------------------------------------------------------------------------

-- Carrier one: log strings to stdout.
newtype LogStdoutC m a
  = LogStdoutC (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects (and also
     -- perform IO)...
     ( Carrier sig m
     , MonadIO m
     )
     -- ... the 'LogStdoutC m' monad can interpret 'Log String :+: sig' effects
  => Carrier (Log String :+: sig) (LogStdoutC m) where

  eff :: (Log String :+: sig) (LogStdoutC m) a -> LogStdoutC m a
  eff = \case
    L (Log message k) ->
      LogStdoutC $ do
        liftIO (putStrLn message)
        runLogStdout k

    R other ->
      LogStdoutC (eff (hmap runLogStdout other))

-- The 'LogStdoutC' runner.
runLogStdout ::
     LogStdoutC m a
  -> m a
runLogStdout (LogStdoutC m) =
  m


-- Carrier two: reinterpret a program that logs 's's into one that logs 't's
-- using a function (provided at runtime) from 's' to 't'.
newtype ReinterpretLogC s t m a
  = ReinterpretLogC { unReinterpretLogC :: ReaderC (s -> t) m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects, one of which
     -- is 'Log t'...
     ( Carrier sig m
     , Member (Log t) sig
     )
     -- ... the 'ReinterpretLogC s t m' monad can interpret 'Log s :+: sig'
     -- effects
  => Carrier (Log s :+: sig) (ReinterpretLogC s t m) where

  eff ::
       (Log s :+: sig) (ReinterpretLogC s t m) a
    -> ReinterpretLogC s t m a
  eff = \case
    L (Log s k) ->
      ReinterpretLogC $ do
        f <- ask @(s -> t)
        log (f s)
        unReinterpretLogC k

    R other ->
      ReinterpretLogC (eff (R (handleCoercible other)))

-- The 'ReinterpretLogC' runner.
reinterpretLog ::
     (s -> t)
  -> ReinterpretLogC s t m a
  -> m a
reinterpretLog f =
  runReader f . unReinterpretLogC



-- Carrier three: collect log messages in a list. This is used for writing this
-- example's test spec.
newtype CollectLogMessagesC s m a
  = CollectLogMessagesC { unCollectLogMessagesC :: WriterC [s] m a }
  deriving newtype (Applicative, Functor, Monad)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects...
     ( Carrier sig m
     , Effect sig
     )
     -- ...the 'CollectLogMessagesC s m' monad can interpret 'Log s :+: sig'
     -- effects
  => Carrier (Log s :+: sig) (CollectLogMessagesC s m) where

  eff ::
       (Log s :+: sig) (CollectLogMessagesC s m) a
    -> CollectLogMessagesC s m a
  eff = \case
    L (Log s k) ->
      CollectLogMessagesC $ do
        tell [s]
        unCollectLogMessagesC k

    R other ->
      CollectLogMessagesC (eff (R (handleCoercible other)))

-- The 'CollectLogMessagesC' runner.
collectLogMessages ::
     CollectLogMessagesC s m a
  -> m ([s], a)
collectLogMessages =
  runWriter . unCollectLogMessagesC


-- Test spec.
spec :: Spec
spec =
  describe "reinterpret log" $
    it "reinterprets logs" $
      ((do
          log (Debug "foo")
          log (Info "bar"))
        & reinterpretLog renderLogMessage
        & collectLogMessages
        & run)
      `shouldBe` (["[debug] foo", "[info] bar"], ())
