{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Catch
( gen
, test
) where

import Control.Effect.Error
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

gen
  :: forall e m a sig
  .  (Has (Catch e) sig m, Arg e, Show a, Show e, Vary e)
  => Gen e
  -> (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen _ m a = label "catchError" catchError <*> m a <*> fn @e (m a)


test
  :: forall e m a b sig
  .  (Has (Error e) sig m, Arg e, Eq a, Eq e, Show a, Show e, Vary e)
  => Gen e
  -> (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Either e a))
  -> [TestTree]
test e m a _ runCatch =
  [ testProperty "catchError intercepts throwError" . forall (e :. fn (m a) :. Nil) $
    \ e h -> runCatch (throwError e `catchError` h) === runCatch (h e)
  ]
