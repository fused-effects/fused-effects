{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype
( example
, runTeletypeIO
) where

import Prelude hiding (read)

import Control.Carrier
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Test.Tasty
import Test.Tasty.QuickCheck

example :: TestTree
example = testGroup "teletype"
  [ testProperty "reads" $
    \ line -> run (runTeletypeRet [line] read) === ([], ([], line))

  , testProperty "writes" $
    \ input output -> run (runTeletypeRet input (write output)) === ([output], (input, ()))

  , testProperty "writes multiple things" $
    \ input output1 output2 -> run (runTeletypeRet input (write output1 >> write output2)) === ([output1, output2], (input, ()))
  ]

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

read :: Has Teletype sig m => m String
read = send (Read pure)

write :: Has Teletype sig m => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  eff (L (Read    k)) = liftIO getLine      >>= k
  eff (L (Write s k)) = liftIO (putStrLn s) >>  k
  eff (R other)       = TeletypeIOC (eff (handleCoercible other))


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  eff (L (Read    k)) = do
    i <- TeletypeRetC get
    case i of
      []  -> k ""
      h:t -> TeletypeRetC (put t) *> k h
  eff (L (Write s k)) = TeletypeRetC (tell [s]) *> k
  eff (R other)       = TeletypeRetC (eff (R (R (handleCoercible other))))
