module Lift
( tests
) where

import           Control.Carrier.State.Strict
import           Control.Effect.Lift
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Lift"
  [ testProperty "liftWith" . property $ do
    r <- liftIO . runState "yep" $ handle (put . getMsg) $ do
      modify ("heck " ++)
      liftIO (E.throwIO (E.AssertionFailed "nope"))
    r === ("nope", ())
  ] where
  getMsg (E.AssertionFailed msg) = msg

handle :: (E.Exception e, Has (Lift IO) sig m) => (e -> m a) -> m a -> m a
handle h m = liftWith $ \ run ctx -> E.handle (run . (<$ ctx) . h) (run (m <$ ctx))
