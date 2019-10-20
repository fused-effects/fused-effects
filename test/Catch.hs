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
  :: forall e m sig
  .  (Has (Catch e) sig m, Arg e, Show e, Vary e)
  => Gen e
  -> GenM m
  -> GenM m
gen _ m a = label "catchError" catchError <*> m a <*> fn @e (m a)


test
  :: (Has (Error e) sig m, Arg e, Eq a, Eq e, Show a, Show e, Vary e, Functor f)
  => Gen e
  -> GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m a _ i (Run runCatch) =
  [ testProperty "catchError intercepts throwError" . forall (i :. e :. fn (m a) :. Nil) $
    \ i e h -> runCatch ((throwError e `catchError` h) <$ i) === runCatch (h e <$ i)
  ]
