{-# LANGUAGE FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype
( example
, runTeletypeIO
) where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import Data.Kind (Type)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude hiding (read)
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

data Teletype (m :: Type -> Type) k where
  Read  ::           Teletype m String
  Write :: String -> Teletype m ()


read :: Has Teletype sig m => m String
read = send Read

write :: Has Teletype sig m => String -> m ()
write s = send (Write s)


runTeletypeIO :: TeletypeIOC m a -> m a
runTeletypeIO = runTeletypeIOC

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hdl sig ctx = case sig of
    L Read      -> (<$ ctx) <$> liftIO getLine
    L (Write s) -> ctx <$ liftIO (putStrLn s)
    R other     -> TeletypeIOC (alg (runTeletypeIOC . hdl) other ctx)


runTeletypeRet :: [String] -> TeletypeRetC m a -> m ([String], ([String], a))
runTeletypeRet i = runWriter . runState i . runTeletypeRetC

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: StateC [String] (WriterC [String] m) a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Teletype :+: sig) (TeletypeRetC m) where
  alg hdl sig ctx = case sig of
    L Read      -> do
      i <- TeletypeRetC get
      case i of
        []  -> pure ("" <$ ctx)
        h:t -> h <$ ctx <$ TeletypeRetC (put t)
    L (Write s) -> ctx <$ TeletypeRetC (tell [s])
    R other     -> TeletypeRetC (alg (runTeletypeRetC . hdl) (R (R other)) ctx)
