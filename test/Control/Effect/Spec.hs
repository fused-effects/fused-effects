{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, MultiWayIf, TemplateHaskell, TypeApplications, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -O2 -fplugin Test.Inspection.Plugin #-}
module Control.Effect.Spec
( spec
) where

import Control.Carrier
import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Prelude hiding (fail)
import Test.Hspec
import Test.Inspection as Inspection

spec :: Spec
spec = do
  inference
  reinterpretation
  fusion

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


reinterpretation :: Spec
reinterpretation = describe "reinterpretation" $ do
  it "can reinterpret effects into other effects" $
    run (runState "a" ((++) <$> reinterpretReader (local ('b':) ask) <*> get)) `shouldBe` ("a", "baa")

reinterpretReader :: ReinterpretReaderC r m a -> StateC r m a
reinterpretReader = runReinterpretReaderC

newtype ReinterpretReaderC r m a = ReinterpretReaderC { runReinterpretReaderC :: StateC r m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance (Carrier sig m, Effect sig) => Carrier (Reader r :+: sig) (ReinterpretReaderC r m) where
  eff (L (Ask       k)) = ReinterpretReaderC get >>= k
  eff (L (Local f m k)) = do
    a <- ReinterpretReaderC get
    ReinterpretReaderC (put (f a))
    v <- m
    ReinterpretReaderC (put a)
    k v
  eff (R other)         = ReinterpretReaderC (eff (R (handleCoercible other)))


shouldSucceed :: Inspection.Result -> Expectation
shouldSucceed (Success _) = pure ()
shouldSucceed (Failure f) = expectationFailure f

fusion :: Spec
fusion = describe "fusion" $ do
  it "eliminates StateCs" $ do
    shouldSucceed $(inspectTest $ 'countDown `doesNotUse` ''StateC)

  it "eliminates nested StateCs" $ do
    shouldSucceed $(inspectTest $ 'countBoth `doesNotUse` ''StateC)

  it "eliminates catch and throw" $ do
    shouldSucceed $(inspectTest $ 'throwing `doesNotUse` ''ErrorC)

  it "eliminates calls to eff" $ do
    shouldSucceed $(inspectTest $ 'countDown `doesNotUse` 'eff)

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
