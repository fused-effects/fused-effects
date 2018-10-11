{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
module Control.Effect.Spec where

import Control.Effect
import Test.Hspec

spec :: Spec
spec = do
  describe "Eff" $ do
    it "can be wrapped for better type inference" $
      run (runHasEnv (runEnv "i" askEnv)) `shouldBe` "i"


askEnv :: (Member (Reader env) sig, Carrier sig m) => HasEnv env m env
askEnv = ask

runEnv :: Carrier sig m => env -> HasEnv env (ReaderH env (HasEnv env m)) a -> HasEnv env m a
runEnv r = runReader r . runHasEnv


newtype HasEnv env carrier a = HasEnv { runHasEnv :: Eff carrier a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig carrier => Carrier sig (HasEnv env carrier) where
  gen = pure
  alg op = HasEnv (alg (handlePure runHasEnv op))

instance (Carrier sig carrier, Effect sig) => Effectful sig (HasEnv env carrier)
