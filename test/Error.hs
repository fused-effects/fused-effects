{-# LANGUAGE RankNTypes #-}
module Error
( tests
, gen
) where

import qualified Control.Carrier.Error.Either as ErrorC
import Control.Effect.Error
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Catch
import Hedgehog
import Hedgehog.Function hiding (C)
import Hedgehog.Gen
import Pure
import qualified Throw
import Test.Tasty

tests :: TestTree
tests = testGroup "Error" $
  [ testGroup "ErrorC"  $ errorTests ErrorC.runError
  , testGroup "Either"  $ errorTests pure
  , testGroup "ExceptT" $ errorTests ExceptT.runExceptT
  ] where
  errorTests :: Has (Error C) sig m => (forall a . m a -> PureC (Either C a)) -> [TestTree]
  errorTests run = Error.errorTests run (genM [gen genC]) genC genA genB


gen :: (Has (Error e) sig m, Arg e, Vary e) => Gen e -> Gen (m a) -> Gen a -> Gen (m a)
gen e ma a = choice
  [ Throw.gen e ma a
  , Catch.gen e ma a
  ]


errorTests :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e) => (forall a . m a -> PureC (Either e a)) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen e -> Gen a -> Gen b -> [TestTree]
errorTests runError m e a b
  =  Throw.throwTests runError m e a b
  ++ Catch.catchTests runError m e a b
