{-# LANGUAGE RankNTypes #-}
module Error
( tests
, gen
, test
) where

import qualified Control.Carrier.Error.Either as ErrorC
import Control.Effect.Error
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Catch
import Data.Functor.Identity (Identity(..))
import Gen
import qualified Monad
import Test.Tasty
import qualified Throw

tests :: TestTree
tests = testGroup "Error" $
  [ test "ErrorC"  ErrorC.runError
  , test "Either"  pure
  , test "ExceptT" ExceptT.runExceptT
  ] where
  test :: Has (Error E) sig m => String -> (forall a . m a -> PureC (Either E a)) -> TestTree
  test name run = testGroup name
    $  Monad.test   (m (gen e)) a b c (pure (Identity ())) (run . runIdentity)
    ++ Error.test e (m (gen e)) a b                        run


gen
  :: (Has (Error e) sig m, Arg e, Show e, Vary e)
  => Gen e
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen e m a = choice
  [ Throw.gen e m a
  , Catch.gen e m a
  ]


test
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e)
  => Gen e
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Either e a))
  -> [TestTree]
test e m a b runError
  =  Throw.test e m a b runError
  ++ Catch.test e m a b runError
