{-# LANGUAGE FlexibleContexts, RankNTypes #-}
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
  [ testGroup "ErrorC"  $
    [ testMonad
    , testError
    ] >>= ($ RunE ErrorC.runError)
  , testGroup "Either"  $ testError (RunE pure)
  , testGroup "ExceptT" $ testError (RunE ExceptT.runExceptT)
  ] where
  testMonad (RunE run) = Monad.test   (m (gen e)) a b c (pure (Identity ())) (liftRunL run)
  testError run        = Error.test e (m (gen e)) a b                                  run


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
  -> RunE e m
  -> [TestTree]
test e m a b runError
  =  Throw.test e m a b runError
  ++ Catch.test e m a b runError
