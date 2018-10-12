{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Teletype where

import Prelude hiding (read)

import Control.Effect
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

read :: (Member Teletype sig, Carrier sig m) => m String
read = send (Read gen)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (gen ()))


runTeletypeIO :: (MonadIO m, Carrier sig m) => Eff (TeletypeIOH m) a -> m a
runTeletypeIO = runTeletypeIOH . interpret

newtype TeletypeIOH m a = TeletypeIOH { runTeletypeIOH :: m a }

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOH m) where
  gen = TeletypeIOH . gen
  alg = algT \/ algOther
    where algT (Read    k) = TeletypeIOH (liftIO getLine >>= runTeletypeIOH . k)
          algT (Write s k) = TeletypeIOH (liftIO (putStrLn s) >> runTeletypeIOH k)
          algOther op = TeletypeIOH (alg (handlePure runTeletypeIOH op))


runTeletypeRet :: (Carrier sig m, Effect sig, Monad m) => [String] -> Eff (TeletypeRetH m) a -> m (([String], [String]), a)
runTeletypeRet s m = runTeletypeRetH (interpret m) s

newtype TeletypeRetH m a = TeletypeRetH { runTeletypeRetH :: [String] -> m (([String], [String]), a) }

instance (Monad m, Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetH m) where
  gen a = TeletypeRetH (\ i -> gen ((i, []), a))
  alg = algT \/ algOther
    where algT (Read    k) = TeletypeRetH (\ i -> case i of
            []  -> runTeletypeRetH (k "") []
            h:t -> runTeletypeRetH (k h)  t)
          algT (Write s k) = TeletypeRetH (\ i -> do
            ((i, out), a) <- runTeletypeRetH k i
            pure ((i, s:out), a))
          algOther op = TeletypeRetH (\ i -> alg (handle ((i, []), ()) mergeResults op))
          mergeResults ((i, o), m) = do
            ((i', o'), a) <- runTeletypeRetH m i
            pure ((i', o ++ o'), a)
