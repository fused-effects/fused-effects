{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer
( tests
, gen
, test
) where

import Control.Arrow ((&&&))
import qualified Control.Carrier.Writer.Strict as StrictWriterC
import Control.Effect.Writer
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriterT
import qualified Control.Monad.Trans.Writer.Strict as StrictWriterT
import Data.Bifunctor (first)
import Data.Tuple (swap)
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ test "WriterC (Strict)" StrictWriterC.runWriter
  , test "WriterT (Lazy)"   (fmap swap . LazyWriterT.runWriterT)
  , test "WriterT (Strict)" (fmap swap . StrictWriterT.runWriterT)
  , test "RWST (Lazy)"      (runRWST LazyRWST.runRWST)
  , test "RWST (Strict)"    (runRWST StrictRWST.runRWST)
  ] where
  test :: Has (Writer W) sig m => String -> (forall a . m a -> PureC (W, a)) -> TestTree
  test s run = testGroup s
    $  Monad.test    (m (gen w b)) a b c run
    ++ Writer.test w (m (gen w b)) a     run
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen
  :: forall w b m a sig
  .  (Has (Writer w) sig m, Arg b, Arg w, Show b, Show w, Vary b, Vary w)
  => Gen w
  -> Gen b
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen w b m a = choice
  [ infixL 4 "<$" (<$) <*> a <*> (label "tell" tell <*> w)
  , atom "fmap" fmap <*> fn a <*> (label "listen" (listen @w) <*> m b)
  , label "censor" censor <*> fn w <*> m a
  ]


test
  :: (Has (Writer w) sig m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w)
  => Gen w
  -> (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> (forall a . m a -> PureC (w, a))
  -> [TestTree]
test w m a runWriter =
  [ testProperty "tell appends a value to the log" . forall (w :. m a :. Nil) $
    \ w m -> runWriter (tell w >> m) === fmap (first (mappend w)) (runWriter m)
  , testProperty "listen eavesdrops on written output" . forall (m a :. Nil) $
    \ m -> runWriter (listen m) === fmap (fst &&& id) (runWriter m)
  , testProperty "censor revises written output" . forall (fn w :. m a :. Nil) $
    \ f m -> runWriter (censor f m) === fmap (first f) (runWriter m)
  ]
