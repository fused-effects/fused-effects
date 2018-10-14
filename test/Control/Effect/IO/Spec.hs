{-# LANGUAGE GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Control.Effect.IO.Spec where

import Control.Effect
import qualified Control.Exception as Exc
import Data.Bifunctor (first)
import Data.Typeable
import Test.Hspec

spec :: Spec
spec = do
  describe "catchIO" $ do
    it "bridges exceptions into Error effects" $ do
      exc <- runM (runError ((error "error" `catchIO` (throwError . Exc.toException @Exc.SomeException)) *> pure ()))
      first extract exc `shouldBe` Left (Just "error")


extract :: Exc.SomeException -> Maybe String
extract (Exc.SomeException (exc :: e)) = case eqT :: Maybe (e :~: Exc.ErrorCall) of
  Just Refl | Exc.ErrorCall str <- exc -> Just str
  _                                    -> Nothing
