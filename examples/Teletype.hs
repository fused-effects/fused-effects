{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures,
  MultiParamTypeClasses, TypeOperators, UndecidableInstances, TypeApplications,
  TypeFamilyDependencies, DataKinds, LambdaCase, ScopedTypeVariables, PolyKinds,
  RankNTypes, DefaultSignatures #-}

module Teletype where

import Prelude hiding (read)

import Control.Effect
import Control.Monad.IO.Class
import Data.Coerce
import Test.Hspec
import Test.Hspec.QuickCheck


-- TESTS -----------------------------------------------------------------------

spec :: Spec
spec = describe "teletype" $ do
  prop "reads" $ \line ->
    run (handleEff @TeletypeRetH read [line])
    `shouldBe`
    (([], []), line)

  prop "writes" $
    \input output ->
      run (handleEff @TeletypeRetH (write output) input)
      `shouldBe`
      ((input, [output]), ())

  prop "writes multiple things" $
    \input output1 output2 ->
      run (handleEff @TeletypeRetH (write output1 >> write output2) input)
      `shouldBe`
      ((input, [output1, output2]), ())

-- GENERIC DESCRIPTIONS OF HANDLERS --------------------------------------------

handleEff
  :: forall handler sig m a. (Handler handler, Carrier sig (handler m))
  => Eff (handler m) a -> Run handler m a
handleEff m = runHandler (interpret @sig m)

class Handler (handler :: (* -> *) -> * -> *) where
  -- All you need to do is instantiate this type family...
  type Run handler (m :: * -> *) (a :: *) :: *

  runHandler :: forall m a. handler m a -> Run handler m a
  default runHandler
    :: Coercible (handler m a) (Run handler m a)
    => handler m a -> Run handler m a
  runHandler = coerce
  handler :: forall m a. Run handler m a -> handler m a
  default handler
    :: Coercible (handler m a) (Run handler m a)
    => Run handler m a -> handler m a
  handler = coerce

-- TELETYPE EFFECT -------------------------------------------------------------

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

-- IO INTERPRETATION OF TELETYPE -----------------------------------------------

newtype TeletypeIOH m a
  = TeletypeIOH (Run TeletypeIOH m a)

instance Handler TeletypeIOH where
  type Run TeletypeIOH m a = m a

instance (MonadIO m, Carrier sig m)
  => Carrier (Teletype :+: sig) (TeletypeIOH m) where

  gen = handler . gen

  alg = algT \/ algOther
    where
      algT = handler . \case
        Read k    -> liftIO getLine >>= runHandler . k
        Write s k -> liftIO (putStrLn s) >> runHandler k

      algOther =
        handler . alg . handlePure (runHandler @TeletypeIOH)

-- PURE INTERPRETATION OF TELETYPE EFFECT --------------------------------------

newtype TeletypeRetH m a
  = TeletypeRetH (Run TeletypeRetH m a)

instance Handler TeletypeRetH where
  type Run TeletypeRetH m a = [String] -> m (([String], [String]), a)

instance (Monad m, Carrier sig m, Effect sig)
  => Carrier (Teletype :+: sig) (TeletypeRetH m) where

  gen a =
    handler $ \i -> gen ((i, []), a)

  alg = algT \/ algOther
    where
      algT = handler . \case
        Read k -> \case
          []  -> runHandler (k "") []
          h:t -> runHandler (k h)  t
        Write s k -> \i -> do
          ((i, out), a) <- runHandler k i
          pure ((i, s:out), a)

      algOther op =
        handler @_ @m $ \i ->
          alg (handle ((i, []), ()) mergeResults op)

      mergeResults ((i, o), m) = do
        ((i', o'), a) <- runHandler m i
        pure ((i', o ++ o'), a)
