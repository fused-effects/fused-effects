{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Error
( tests
, gen0
, genN
, test
) where

import qualified Catch
import qualified Control.Carrier.Error.Church as C.Church
import qualified Control.Carrier.Error.Cont as C.Cont
import qualified Control.Carrier.Error.CPS as C.CPS
import qualified Control.Carrier.Error.Either as C.Either
import           Control.Effect.Error
import qualified Control.Monad.Trans.Except as T.Except
import           Data.Semigroup as S ((<>))
import           Gen
import qualified Monad
import qualified MonadFix
import           Test.Tasty
import qualified Throw

tests :: TestTree
tests = testGroup "Error"
  [ testGroup "ErrorC (Church)" $
    [ testMonad
    , testMonadFix
    , testError
    ] >>= ($ runL (C.Church.runError (pure . Left) (pure . Right)))
  , testGroup "ErrorC (Cont)" $
    [ testMonad
    , testMonadFix
    , testError
    ] >>= ($ runL (C.Cont.runError (pure . Right)))
  , testGroup "ErrorC (CPS)" $
    [ testMonad
    , testMonadFix
    , testError
    ] >>= ($ runL (C.CPS.runError pure))
  , testGroup "ErrorC (Either)" $
    [ testMonad
    , testMonadFix
    , testError
    ] >>= ($ runL C.Either.runError)
  , testGroup "Either"  $ testError (runL pure)
  , testGroup "ExceptT" $ testError (runL T.Except.runExceptT)
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
