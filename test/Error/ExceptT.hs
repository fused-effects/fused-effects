{-# LANGUAGE TypeApplications #-}
module Error.ExceptT
( tests
) where

import Control.Effect.Error
import qualified Control.Monad.Trans.Except as Except
import Error
import Hedgehog.Function hiding (C)
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.ExceptT"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (genM [genError genC] genB) :. Nil) $
    \ e k -> throwError_annihilation (~=) (Except.runExceptT @C) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (genM [genError genC] genA) :. Nil) $
    \ e f -> catchError_interception (~=) (Except.runExceptT @C) e (apply f)
  ]
