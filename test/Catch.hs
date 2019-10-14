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

gen :: forall e m a sig . (Has (Catch e) sig m, Arg e, Vary e) => Gen e -> Gen (m a) -> Gen a -> Gen (m a)
gen _ ma _ = choice [ fn @e ma >>= subterm ma . flip catchError . apply ]


catchTests :: forall e m a b sig . (Has (Error e) sig m, Arg e, Eq a, Eq e, Show a, Show e, Vary e) => (forall a . m a -> PureC (Either e a)) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen e -> Gen a -> Gen b -> [TestTree]
catchTests runError m e a _ =
  [ testProperty "catchError interception" . forall (e :. fn (m a) :. Nil) $
    \ e f -> catchError_interception (~=) runError e (getBlind . apply f)
  ]
