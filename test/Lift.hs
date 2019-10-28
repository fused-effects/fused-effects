{-# LANGUAGE TypeApplications #-}
module Lift
( tests
) where

import Control.Carrier.State.Strict
import Control.Effect.Lift
import qualified Control.Exception as E
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Lift"
  [ testCase "liftWith" $ do
    r <- liftIO . runState "yep" $ liftWith $ \ ctx hdl ->
      E.handle (hdl . (<$ ctx) . put . getMsg) $
        hdl (liftIO (E.throwIO (E.AssertionFailed "nope")) <$ ctx)
    r @?= ("nope", ())
  ] where
  getMsg (E.AssertionFailed msg) = msg
