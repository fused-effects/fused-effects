{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype
( example
, runTeletypeIO
) where

import Prelude hiding (read)

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

example :: TestTree
example = testGroup "teletype"
  [ testProperty "reads" . property $ do
    line <- forAll genLine
    run (runTeletypeRet [line] read) === ([], ([], line))

  , testProperty "writes" . property $ do
    input  <- forAll (Gen.list (Range.linear 0 10) genLine)
    output <- forAll genLine
    run (runTeletypeRet input (write output)) === ([output], (input, ()))

  , testProperty "writes multiple things" . property $ do
    input   <- forAll (Gen.list (Range.linear 0 10) genLine)
    output1 <- forAll genLine
    output2 <- forAll genLine
    run (runTeletypeRet input (write output1 >> write output2)) === ([output1, output2], (input, ()))
  ] where
  genLine = Gen.string (Range.linear 0 20) Gen.unicode

data Teletype m k
  = Read (String -> m k)
  | Write String (m k)
  deriving (Functor, Generic1)

instance Effect Teletype

read :: Has Teletype sig m => m String
read = send (Read pure)

write :: Has Teletype sig m => String -> m ()
write s = send (Write s (pure ()))


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hom = \case
    L (Read    k) -> liftIO getLine      >>= hom . k
    L (Write s k) -> liftIO (putStrLn s) >>  hom k
    R other       -> TeletypeIOC (alg (runTeletypeIOC . hom) other)


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving (Applicative, Functor, Monad)

instance (Algebra sig m, Effect sig) => Algebra (Teletype :+: sig) (TeletypeRetC m) where
  alg hom = \case
    L (Read    k) -> do
      i <- TeletypeRetC get
      case i of
        []  -> hom (k "")
        h:t -> TeletypeRetC (put t) *> hom (k h)
    L (Write s k) -> TeletypeRetC (tell [s]) *> hom k
    R other       -> TeletypeRetC (alg (runTeletypeRetC . hom) (R (R other)))
