{-# LANGUAGE TypeApplications #-}
module Error.ExceptT
( tests
) where

import Control.Effect.Error
import qualified Control.Monad.Trans.Except as Except
import qualified Error
import Hedgehog.Function hiding (C)
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Error.ExceptT"
  [ testProperty "throwError annihilation" . forall (genC :. fn @A (Error.gen genC genB) :. Nil) $
    \ e k -> throwError_annihilation (~=) (Except.runExceptT @C) e (apply k)
  , testProperty "catchError interception" . forall (genC :. fn @C (Error.gen genC genA) :. Nil) $
    \ e f -> catchError_interception (~=) (Except.runExceptT @C) e (apply f)
  ]
