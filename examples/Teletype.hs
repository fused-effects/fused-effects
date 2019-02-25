{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype where

import Prelude hiding (read)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Monad.IO.Class
import Data.Coerce
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "teletype" $ do
  prop "reads" $
    \ line -> run (runTeletypeRet [line] read) `shouldBe` (([], []), line)

  prop "writes" $
    \ input output -> run (runTeletypeRet input (write output)) `shouldBe` ((input, [output]), ())

  prop "writes multiple things" $
    \ input output1 output2 -> run (runTeletypeRet input (write output1 >> write output2)) `shouldBe` ((input, [output1, output2]), ())

data Teletype (m :: * -> *) k
  = Read (String -> k)
  | Write String k
  deriving (Functor)

instance HFunctor Teletype where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Teletype where
  handle state handler (Read    k) = Read (handler . (<$ state) . k)
  handle state handler (Write s k) = Write s (handler (k <$ state))

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff = handleSum (TeletypeIOC . eff . handleCoercible) (\case
    Read    k -> liftIO getLine      >>= k
    Write s k -> liftIO (putStrLn s) >>  k)


runTeletypeRet :: [String] -> TeletypeRetC m a -> m (([String], [String]), a)
runTeletypeRet = flip runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: [String] -> m (([String], [String]), a) }
  deriving (Functor)

instance Monad m => Applicative (TeletypeRetC m) where
  pure a = TeletypeRetC (\ i -> pure ((i, []), a))
  TeletypeRetC f <*> TeletypeRetC a = TeletypeRetC (\ i1 -> do
    ((i2, o1), f') <- f i1
    ((i3, o2), a') <- a i2
    pure ((i3, o1 ++ o2), f' a'))

instance Monad m => Monad (TeletypeRetC m) where
  TeletypeRetC a >>= f = TeletypeRetC (\ i -> a i >>= \ ((i', o), a') -> runTeletypeRetC (f a') i')

instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff op = TeletypeRetC (\ i -> handleSum (eff . handle ((i, []), ()) mergeResults) (\case
    Read k -> case i of
      []  -> runTeletypeRetC (k "") []
      h:t -> runTeletypeRetC (k h)  t
    Write s k -> do
      ((i, out), a) <- runTeletypeRetC k i
      pure ((i, s:out), a)) op)
    where mergeResults ((i, o), m) = do
            ((i', o'), a) <- runTeletypeRetC m i
            pure ((i', o ++ o'), a)
