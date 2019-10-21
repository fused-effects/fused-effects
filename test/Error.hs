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
    ] >>= ($ runL ErrorC.runError)
  , testGroup "Either"  $ testError (runL pure)
  , testGroup "ExceptT" $ testError (runL ExceptT.runExceptT)
  ] where
  testMonad    run = Monad.test    (m (gen e)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen e)) a b   (identity <*> unit) run
  testError    run = Error.test e  (m (gen e)) a b   (identity <*> unit) run


gen :: (Has (Error e) sig m, Arg e, Show e, Vary e) => Gen e -> GenM m -> GenM m
gen e = choiceM
  [ Throw.gen e
  , Catch.gen e
  ]


test
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e, Functor f)
  => Gen e
  -> GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m a b i runError
  =  Throw.test e m a b i runError
  ++ Catch.test e m a b i runError
