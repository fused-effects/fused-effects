{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Catch
( gen
, catchTests
) where

import Control.Effect.Error
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

gen :: forall e m a sig . (Has (Catch e) sig m, Arg e, Show a, Show e, Vary e) => Gen e -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen _ m a = do
  h <- fn @e (m a)
  subterm (m a) $ \ m -> liftWith2 "catchError" catchError m (fmap getWith <$> showingFn h)


catchTests :: forall e m a b sig . (Has (Error e) sig m, Arg e, Eq a, Eq e, Show a, Show e, Vary e) => (forall a . m a -> PureC (Either e a)) -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen e -> Gen a -> Gen b -> [TestTree]
catchTests runError m e a _ =
  [ testProperty "catchError intercepts throwError" . forall (e :. fn (m a) :. Nil) $
    \ e (FnWith h) -> catchError_interception (===) runError e h
  ]
