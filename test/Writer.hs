{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer
( tests
, gen
, test
) where

import Control.Arrow ((&&&))
import qualified Control.Carrier.Writer.Strict as WriterC
import Control.Effect.Writer
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWST
import qualified Control.Monad.Trans.RWS.Strict as StrictRWST
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriterT
import qualified Control.Monad.Trans.Writer.Strict as StrictWriterT
import Data.Bifunctor (first)
import Data.Tuple (swap)
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Writer"
  [ testGroup "WriterC" $
    [ testMonad
    , testMonadFix
    , testWriter
    ] >>= ($ RunL WriterC.runWriter)
  , testGroup "(,)"              $ testWriter (RunL pure)
  , testGroup "WriterT (Lazy)"   $ testWriter (RunL (fmap swap . LazyWriterT.runWriterT))
  , testGroup "WriterT (Strict)" $ testWriter (RunL (fmap swap . StrictWriterT.runWriterT))
  , testGroup "RWST (Lazy)"      $ testWriter (RunL (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"    $ testWriter (RunL (runRWST StrictRWST.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen w b)) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (gen w b)) a b   (identity <*> unit) run
  testWriter   run = Writer.test w (m (gen w b)) a                         run
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen
  :: forall w b m sig
  .  (Has (Writer w) sig m, Arg b, Arg w, Show b, Show w, Vary b, Vary w)
  => Gen w
  -> Gen b
  -> GenM m
  -> GenM m
gen w b m a = choice
  [ infixL 4 "<$" (<$) <*> a <*> (label "tell" tell <*> w)
  , atom "fmap" fmap <*> fn a <*> (label "listen" (listen @w) <*> m b)
  , label "censor" censor <*> fn w <*> m a
  ]


test
  :: (Has (Writer w) sig m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w)
  => Gen w
  -> GenM m
  -> Gen a
  -> RunL ((,) w) m
  -> [TestTree]
test w m a (RunL runWriter) =
  [ testProperty "tell appends a value to the log" . forall (w :. m a :. Nil) $
    \ w m -> runWriter (tell w >> m) === fmap (first (mappend w)) (runWriter m)
  , testProperty "listen eavesdrops on written output" . forall (m a :. Nil) $
    \ m -> runWriter (listen m) === fmap (fst &&& id) (runWriter m)
  , testProperty "censor revises written output" . forall (fn w :. m a :. Nil) $
    \ f m -> runWriter (censor f m) === fmap (first f) (runWriter m)
  ]
