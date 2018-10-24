{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances #-}
module Control.Effect.Spec where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Fail
import Control.Effect.Interpose
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Sum
import Prelude hiding (fail)
import Test.Hspec

spec :: Spec
spec = do
  inference
  reinterpretation
  interception
  interposition


inference :: Spec
inference = describe "inference" $ do
  it "can be wrapped for better type inference" $
    run (runHasEnv (runEnv "i" ((++) <$> askEnv <*> askEnv))) `shouldBe` "ii"

askEnv :: (Member (Reader env) sig, Carrier sig m) => HasEnv env m env
askEnv = ask

runEnv :: Carrier sig m => env -> HasEnv env (ReaderC env (Eff m)) a -> HasEnv env m a
runEnv r = HasEnv . runReader r . runHasEnv


newtype HasEnv env carrier a = HasEnv { runHasEnv :: Eff carrier a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig carrier => Carrier sig (HasEnv env carrier) where
  ret = pure
  eff op = HasEnv (eff (handleCoercible op))


reinterpretation :: Spec
reinterpretation = describe "reinterpretation" $ do
  it "can reinterpret effects into other effects" $
    run (runState "a" ((++) <$> reinterpretReader (local ('b':) ask) <*> get)) `shouldBe` ("a", "baa")

reinterpretReader :: (Carrier sig m, Effect sig) => Eff (ReinterpretReaderC r m) a -> Eff (StateC r m) a
reinterpretReader = runReinterpretReaderC . interpret

newtype ReinterpretReaderC r m a = ReinterpretReaderC { runReinterpretReaderC :: Eff (StateC r m) a }

instance (Carrier sig m, Effect sig) => Carrier (Reader r :+: sig) (ReinterpretReaderC r m) where
  ret = ReinterpretReaderC . ret
  eff = ReinterpretReaderC . (alg \/ eff . R . handleCoercible)
    where alg (Ask       k) = get >>= runReinterpretReaderC . k
          alg (Local f m k) = do
            a <- get
            put (f a)
            v <- runReinterpretReaderC m
            put a
            runReinterpretReaderC (k v)


interception :: Spec
interception = describe "interception" $ do
  it "can intercept handlers without changing the available effects" $
    run (runFail (interceptFail (fail "world"))) `shouldBe` (Left "hello, world" :: Either String Int)

  it "interception only intercepts effects in its scope" $ do
    run (runFail (fail "world" *> interceptFail (pure (0 :: Int)))) `shouldBe` Left "world"
    run (runFail (interceptFail (pure (0 :: Int)) <* fail "world")) `shouldBe` Left "world"

interceptFail :: (Member Fail sig, Carrier sig m) => Eff (InterceptC m) a -> m a
interceptFail = runInterceptC . interpret

newtype InterceptC m a = InterceptC { runInterceptC :: m a }

instance (Member Fail sig, Carrier sig m) => Carrier sig (InterceptC m) where
  ret = InterceptC . ret
  eff op
    | Just (Fail s) <- prj op = InterceptC (send (Fail ("hello, " ++ s)))
    | otherwise               = InterceptC (eff (handleCoercible op))


interposition :: Spec
interposition = describe "interposition" $ do
  it "can interpose handlers without changing the available effects" $
    run (runFail (runInterpose @Fail (interposeFail (fail "world")))) `shouldBe` (Left "hello, world" :: Either String Int)

  it "interposition only intercepts effects in its scope" $ do
    run (runFail (runInterpose @Fail (fail "world" *> interposeFail (pure (0 :: Int))))) `shouldBe` Left "world"
    run (runFail (runInterpose @Fail (interposeFail (pure (0 :: Int)) <* fail "world"))) `shouldBe` Left "world"

interposeFail :: (Member Fail sig, Member (Interpose Fail) sig, Carrier sig m) => m a -> m a
interposeFail m = interpose m $ \ (Fail s) -> send (Fail ("hello, " ++ s))
