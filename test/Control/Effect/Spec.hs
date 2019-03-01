{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Spec where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Fail
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Sum
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

runEnv :: Carrier sig m => env -> HasEnv env (ReaderC env m) a -> HasEnv env m a
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


interposition :: Spec
interposition = describe "interposition" $ do
  it "can interpose handlers without changing the available effects" $
    run (runFail (interposeFail (fail "world"))) `shouldBe` (Left "hello, world" :: Either String Int)

  it "interposition only intercepts effects in its scope" $ do
    run (runFail (fail "world" *> interposeFail (pure (0 :: Int)))) `shouldBe` Left "world"
    run (runFail (interposeFail (pure (0 :: Int)) <* fail "world")) `shouldBe` Left "world"

interposeFail :: InterposeC m a -> m a
interposeFail = runInterposeC

newtype InterposeC m a = InterposeC { runInterposeC :: m a }
  deriving (Applicative, Functor, Monad)

instance (Carrier sig m, Member Fail sig) => MonadFail (InterposeC m) where
  fail s = send (Fail s)

instance (Carrier sig m, Member Fail sig) => Carrier sig (InterposeC m) where
  eff op
    | Just (Fail s) <- prj op = InterposeC (send (Fail ("hello, " ++ s)))
    | otherwise               = InterposeC (eff (handleCoercible op))
