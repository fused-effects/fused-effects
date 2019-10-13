{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Error
( genError
, testError
) where

import Control.Effect.Error
import qualified Catch
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import qualified Throw
import Test.Tasty
import Test.Tasty.Hedgehog

genError :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen a -> Gen (m a) -> Gen (m a)
genError e a ma = choice
  [ Throw.genThrow e a ma
  , Catch.genCatch e a ma
  ]


testError :: forall e m a b sig . (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e) => String -> (forall a . m a -> PureC (Either e a)) -> Gen e -> Gen a -> Gen b -> TestTree
testError name runError e a b = testGroup name
  [ testProperty "throwError annihilation" . forall (e :. fn @a (Blind <$> genM [genError e] b) :. Nil) $
    \ e k -> throwError_annihilation (~=) runError e (getBlind . apply k)
  , testProperty "catchError interception" . forall (e :. fn (Blind <$> genM [genError e] a) :. Nil) $
    \ e f -> catchError_interception (~=) runError e (getBlind . apply f)
  ]
