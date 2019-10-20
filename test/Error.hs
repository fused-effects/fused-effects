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
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import qualified Throw

tests :: TestTree
tests = testGroup "Error" $
  [ testGroup "ErrorC"  $
    [ testMonad
    , testMonadFix
    , testError
    ] >>= ($ RunL ErrorC.runError)
  , testGroup "Either"  $ testError (RunL pure)
  , testGroup "ExceptT" $ testError (RunL ExceptT.runExceptT)
  ] where
  testMonad    run = Monad.test    (m (gen e)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen e)) a b   (identity <*> unit) run
  testError    run = Error.test e  (m (gen e)) a b                       run


gen :: (Has (Error e) sig m, Arg e, Show e, Vary e) => Gen e -> GenM m -> GenM m
gen e m a = choice
  [ Throw.gen e m a
  , Catch.gen e m a
  ]


test
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e)
  => Gen e
  -> GenM m
  -> Gen a
  -> Gen b
  -> RunL (Either e) m
  -> [TestTree]
test e m a b runError
  =  Throw.test e m a b runError
  ++ Catch.test e m a b runError
