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
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ testGroup "WriterC (Strict)" $ test w (m (gen w b)) a StrictWriterC.runWriter
  , testGroup "WriterT (Lazy)"   $ test w (m (gen w b)) a (fmap swap . LazyWriterT.runWriterT)
  , testGroup "WriterT (Strict)" $ test w (m (gen w b)) a (fmap swap . StrictWriterT.runWriterT)
  , testGroup "RWST (Lazy)"      $ test w (m (gen w b)) a (runRWST LazyRWST.runRWST)
  , testGroup "RWST (Strict)"    $ test w (m (gen w b)) a (runRWST StrictRWST.runRWST)
  ] where
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen
  :: forall w b m a sig
  .  (Has (Writer w) sig m, Arg b, Arg w, Show a, Show b, Show w, Vary b, Vary w)
  => Gen w
  -> Gen b
  -> (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen w b m a = choice
  [ infixL 4 "<$" (<$) <*> a <*> addLabel "tell" (liftWith "tell" tell w)
  , atom "fmap" fmap <*> fn a <*> addLabel "listen" (liftWith "listen" (listen @w) (m b))
  , addLabel "censor" (atom "censor" censor <*> fn w <*> m a)
  ]


test
  :: (Has (Writer w) sig m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w)
  => Gen w
  -> (forall a . Show a => Gen a -> Gen (m a))
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
