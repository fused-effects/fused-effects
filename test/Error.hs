{-# LANGUAGE RankNTypes #-}
module Error
( tests
, gen
, errorTests
) where

import qualified Control.Carrier.Error.Either as ErrorC
import Control.Effect.Error
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Catch
import Hedgehog
import Hedgehog.Function
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
  errorTests :: Has (Error E) sig m => (forall a . m a -> PureC (Either E a)) -> [TestTree]
  errorTests run = Error.errorTests run (m (gen e)) e a b


gen :: (Has (Error e) sig m, Arg e, Show a, Show e, Vary e) => Gen e -> (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))
gen e m a = choice
  [ Throw.gen e m a
  , Catch.gen e m a
  ]


errorTests
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e)
  => (forall a . m a -> PureC (Either e a))
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen e
  -> Gen a
  -> Gen b
  -> [TestTree]
errorTests runError m e a b
  =  Throw.test e m a b runError
  ++ Catch.test e m a b runError
