{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype where

import Prelude hiding (read)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.State
import Control.Effect.Sum
import Control.Effect.Writer
import Control.Monad.IO.Class
import Data.Coerce
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "teletype" $ do
  prop "reads" $
    \ line -> run (runTeletypeRet [line] read) `shouldBe` ([], ([], line))

  prop "writes" $
    \ input output -> run (runTeletypeRet input (write output)) `shouldBe` ([output], (input, ()))

  prop "writes multiple things" $
    \ input output1 output2 -> run (runTeletypeRet input (write output1 >> write output2)) `shouldBe` ([output1, output2], (input, ()))

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving (Functor)

instance HFunctor Teletype where
  hmap f (Read    k) = Read    (f . k)
  hmap f (Write m k) = Write m (f   k)

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read pure)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff (L (Read    k)) = liftIO getLine      >>= k
  eff (L (Write s k)) = liftIO (putStrLn s) >>  k
  eff (R other)       = TeletypeIOC (eff (handleCoercible other))


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  eff (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  eff (R other)       = TeletypeRetC (eff (R (R (handleCoercible other))))
