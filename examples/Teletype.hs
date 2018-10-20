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
read = send (Read ret)

write :: (Member Teletype sig, Carrier sig m) => String -> m ()
write s = send (Write s (ret ()))


runTeletypeIO :: (MonadIO m, Carrier sig m) => Eff (TeletypeIOC m) a -> m a
runTeletypeIO = runTeletypeIOC . interpret

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIOC :: m a }

instance (MonadIO m, Carrier sig m) => Carrier (Teletype :+: sig) (TeletypeIOC m) where
  ret = TeletypeIOC . ret
  eff = alg \/ algOther
    where alg (Read    k) = TeletypeIOC (liftIO getLine >>= runTeletypeIOC . k)
          alg (Write s k) = TeletypeIOC (liftIO (putStrLn s) >> runTeletypeIOC k)
          algOther op = TeletypeIOC (eff (handlePure runTeletypeIOC op))


runTeletypeRet :: (Carrier sig m, Effect sig, Monad m) => [String] -> Eff (TeletypeRetC m) a -> m (([String], [String]), a)
runTeletypeRet s m = runTeletypeRetC (interpret m) s

newtype TeletypeRetC m a = TeletypeRetC { runTeletypeRetC :: [String] -> m (([String], [String]), a) }

instance (Monad m, Carrier sig m, Effect sig) => Carrier (Teletype :+: sig) (TeletypeRetC m) where
  ret a = TeletypeRetC (\ i -> ret ((i, []), a))
  eff = alg \/ algOther
    where alg (Read    k) = TeletypeRetC (\ i -> case i of
            []  -> runTeletypeRetC (k "") []
            h:t -> runTeletypeRetC (k h)  t)
          alg (Write s k) = TeletypeRetC (\ i -> do
            ((i, out), a) <- runTeletypeRetC k i
            pure ((i, s:out), a))
          algOther op = TeletypeRetC (\ i -> eff (handle ((i, []), ()) mergeResults op))
          mergeResults ((i, o), m) = do
            ((i', o'), a) <- runTeletypeRetC m i
            pure ((i', o ++ o'), a)
