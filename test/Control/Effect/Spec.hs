{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Spec where

import Control.Effect
import Prelude hiding (fail)
import Test.Hspec

spec :: Spec
spec = do
  inference
  reinterpretation
  interposition


inference :: Spec
inference = describe "inference" $ do
  it "can be wrapped for better type inference" $
    run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv))) `shouldBe` "ii"

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


reinterpretation :: Spec
reinterpretation = describe "reinterpretation" $ do
  it "can reinterpret effects into other effects" $
    run (runState "a" ((++) <$> reinterpretReader (local ('b':) ask) <*> get)) `shouldBe` ("a", "baa")

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


interposition :: Spec
interposition = describe "interposition" $ do
  it "can interpose handlers without changing the available effects" $
    run (runFail (interposeFail (fail "world"))) `shouldBe` (Left "hello, world" :: Either String Int)

  it "interposition only intercepts effects in its scope" $ do
    run (runFail (fail "world" *> interposeFail (pure (0 :: Int)))) `shouldBe` Left "world"
    run (runFail (interposeFail (pure (0 :: Int)) <* fail "world")) `shouldBe` Left "world"

interposeFail :: (Member Fail sig, Carrier sig m) => Eff (InterposeH m) a -> m a
interposeFail = runInterposeH . interpret

newtype InterposeH m a = InterposeH { runInterposeH :: m a }

instance (Member Fail sig, Carrier sig m) => Carrier sig (InterposeH m) where
  gen = InterposeH . gen
  alg op
    | Just (Fail s) <- prj op = InterposeH (send (Fail ("hello, " ++ s)))
    | otherwise               = InterposeH (alg (handlePure runInterposeH op))
