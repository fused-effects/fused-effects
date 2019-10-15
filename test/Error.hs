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
import Gen
import qualified Throw
import Test.Tasty

tests :: TestTree
tests = testGroup "Error" $
  [ testGroup "ErrorC"  $ test e (m (gen e)) a b ErrorC.runError
  , testGroup "Either"  $ test e (m (gen e)) a b pure
  , testGroup "ExceptT" $ test e (m (gen e)) a b ExceptT.runExceptT
  ]


gen
  :: (Has (Error e) sig m, Arg e, Show a, Show e, Vary e)
  => Gen e
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen (With (m a))
gen e m a = choice
  [ Throw.gen e m a
  , Catch.gen e m a
  ]


test
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e)
  => Gen e
  -> (forall a . Show a => Gen a -> Gen (With (m a)))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC (Either e a))
  -> [TestTree]
test e m a b runError
  =  Throw.test e m a b runError
  ++ Catch.test e m a b runError
