{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeInType, UndecidableInstances #-}
module Control.Effect.LiftIO
  (spec)
where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Effect
import Data.Coerce
import Control.Effect.Lift
import Control.Effect.Carrier
import Control.Effect.Reader
import Prelude hiding (fail)
import Test.Hspec

spec :: Spec
spec = do
  liftio

liftio :: Spec
liftio = describe "liftio" $ do
  it "can run liftIO with the underlying monad not being IO" $
    doliftio >>= (`shouldBe` ())

doliftio :: MonadIO m => m ()
doliftio = runMIO (liftIO $ putStr "")
