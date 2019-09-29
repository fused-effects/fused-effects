{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, MultiWayIf, TemplateHaskell, TypeApplications, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
module Control.Effect.Spec
( spec
, tests
) where

import Control.Carrier
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Test.Hspec
import Test.Inspection as Inspection
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Effect"
  [ fusion
  ]


spec :: Spec
spec = do
  inference

inference :: Spec
inference = describe "inference" $ do
  it "can be wrapped for better type inference" $
    run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv))) `shouldBe` "ii"

askEnv :: Has (Reader env) sig m => HasEnv env m env
askEnv = ask

runEnv :: env -> HasEnv env (ReaderC env m) a -> HasEnv env m a
runEnv r = HasEnv . runReader r . runHasEnv


newtype HasEnv env m a = HasEnv { runHasEnv :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig m => Carrier sig (HasEnv env m) where
  eff = HasEnv . eff . handleCoercible


failureOf :: Inspection.Result -> Maybe String
failureOf (Success _) = Nothing
failureOf (Failure f) = Just f

fusion :: TestTree
fusion = testGroup "fusion"
  [ testCase "eliminates StateCs" $
    failureOf $(inspectTest $ 'countDown `doesNotUse` ''StateC)
    @?= Nothing
  , testCase "eliminates nested StateCs" $
    failureOf $(inspectTest $ 'countBoth `doesNotUse` ''StateC)
    @?= Nothing
  , testCase "eliminates catch and throw" $
    failureOf $(inspectTest $ 'throwing `doesNotUse` ''ErrorC)
    @?= Nothing
  , testCase "eliminates calls to eff" $
    failureOf $(inspectTest $ 'countDown `doesNotUse` 'eff)
    @?= Nothing
  ]

countDown :: Int -> (Int, Int)
countDown start = run . runState start $ go
  where go = get >>= \n -> if n <= 0 then pure n else modify @Int pred *> go

countBoth :: Int -> (Int, (Float, ()))
countBoth n = run . runState n . runState (fromIntegral n) $ go where
  go = do
    n <- get @Int
    if
      | n == 0         -> pure ()
      | n `mod` 2 == 0 -> modify @Float (+ 1) *> modify @Int pred *> go
      | otherwise      -> modify @Int pred    *> go

throwing :: Int -> Either Int String
throwing n = run $ runError go
  where go = if n > 10 then throwError @Int 42 else pure "fine"
