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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ReinterpretLog
  ( example
  , application
  , runApplication
  ) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Data.Kind (Type)
import Prelude hiding (log)
import Test.Tasty
import Test.Tasty.HUnit


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
application :: Has (Log Message) sig m
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
    --   ReinterpretLogC Message String (LogStdoutC IO) ()

    & reinterpretLog renderLogMessage
    -- Now its type is:
    --
    --   LogStdoutC IO ()

    & runLogStdout
    -- Now its type is:
    --
    --   IO ()


--------------------------------------------------------------------------------
-- The logging effect
--------------------------------------------------------------------------------

-- Log an 'a', then continue with 'k'.
data Log (a :: Type) (m :: Type -> Type) (k :: Type) where
  Log :: a -> Log a m ()


-- Log an 'a'.
log :: Has (Log a) sig m
  => a
  -> m ()
log x =
  send (Log x)


--------------------------------------------------------------------------------
-- The logging effect carriers
--------------------------------------------------------------------------------

-- Carrier one: log strings to stdout.
newtype LogStdoutC m a
  = LogStdoutC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects (and also
     -- perform IO)...
     ( Algebra sig m
     , MonadIO m
     )
     -- ... the 'LogStdoutC m' monad can interpret 'Log String :+: sig' effects
  => Algebra (Log String :+: sig) (LogStdoutC m) where

  alg hdl sig ctx = case sig of
    L (Log message) ->
      ctx <$ LogStdoutC (liftIO (putStrLn message))

    R other ->
      LogStdoutC (alg (runLogStdout . hdl) other ctx)

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
  deriving (Applicative, Functor, Monad, MonadIO)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects, one of which
     -- is 'Log t'...
     Has (Log t) sig m
     -- ... the 'ReinterpretLogC s t m' monad can interpret 'Log s :+: sig'
     -- effects
  => Algebra (Log s :+: sig) (ReinterpretLogC s t m) where

  alg hdl sig ctx = case sig of
    L (Log s) ->
      ReinterpretLogC $ do
        f <- ask @(s -> t)
        ctx <$ log (f s)

    R other ->
      ReinterpretLogC (alg (unReinterpretLogC . hdl) (R other) ctx)

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
  deriving (Applicative, Functor, Monad)

instance
     -- So long as the 'm' monad can interpret the 'sig' effects...
     Algebra sig m
     -- ...the 'CollectLogMessagesC s m' monad can interpret 'Log s :+: sig'
     -- effects
  => Algebra (Log s :+: sig) (CollectLogMessagesC s m) where

  alg hdl sig ctx = case sig of
    L (Log s) ->
      ctx <$ CollectLogMessagesC (tell [s])

    R other ->
      CollectLogMessagesC (alg (unCollectLogMessagesC . hdl) (R other) ctx)

-- The 'CollectLogMessagesC' runner.
collectLogMessages ::
     CollectLogMessagesC s m a
  -> m ([s], a)
collectLogMessages =
  runWriter . unCollectLogMessagesC


-- Test spec.
example :: TestTree
example = testGroup "reinterpret log"
  [ testCase "reinterprets logs" $
    ((do
        log (Debug "foo")
        log (Info "bar"))
      & reinterpretLog renderLogMessage
      & collectLogMessages
      & run)
    @?= (["[debug] foo", "[info] bar"], ())
  ]
