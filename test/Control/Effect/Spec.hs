{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Spec where

import Control.Effect
import Test.Hspec

spec :: Spec
spec = do
  describe "Eff" $ do
    it "can be wrapped for better type inference" $
      run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv))) `shouldBe` "ii"

    it "can reinterpret effects into other effects" $
      run (runState "a" ((++) <$> reinterpretReader (local ('b':) ask) <*> get)) `shouldBe` ("a", "baa")


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


reinterpretReader :: (Effectful (State r :+: sig) m, Effect sig) => Eff (ReinterpretReaderH r m) a -> m a
reinterpretReader = runReinterpretReaderH . interpret

newtype ReinterpretReaderH r m a = ReinterpretReaderH { runReinterpretReaderH :: m a }

instance (Effectful (State r :+: sig) m, Effect sig) => Carrier (Reader r :+: sig) (ReinterpretReaderH r m) where
  gen = ReinterpretReaderH . gen
  alg = algR \/ (ReinterpretReaderH . alg . R . handlePure runReinterpretReaderH)
    where algR (Ask       k) = ReinterpretReaderH (get >>= runReinterpretReaderH . k)
          algR (Local f m k) = ReinterpretReaderH $ do
            a <- get
            put (f a)
            v <- runReinterpretReaderH m
            put a
            runReinterpretReaderH (k v)
