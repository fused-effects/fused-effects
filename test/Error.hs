{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Error
( tests
, gen0
, genN
, test
) where

import qualified Control.Carrier.Error.Either as ErrorC
import Control.Effect.Error
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Catch
import Data.Semigroup as S ((<>))
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
  testMonad    run = Monad.test    (m (gen0 e) (genN e)) a b c initial run
  testMonadFix run = MonadFix.test (m (gen0 e) (genN e)) a b   initial run
  testError    run = Error.test e  (m (gen0 e) (genN e)) a b   initial run
  initial = identity <*> unit

gen0 :: Has (Error e) sig m => GenTerm e -> GenTerm a -> [GenTerm (m a)]
gen0 = Throw.gen0

genN
  :: (Has (Error e) sig m, Arg e, Show e, Vary e)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN = Catch.genN


test
  :: (Has (Error e) sig m, Arg a, Arg e, Eq a, Eq b, Eq e, Show a, Show b, Show e, Vary a, Vary e, Functor f)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m = Throw.test e m S.<> Catch.test e m
