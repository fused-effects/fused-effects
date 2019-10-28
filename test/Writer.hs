{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Writer
( tests
, gen0
, genN
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
    ] >>= ($ runL WriterC.runWriter)
  , testGroup "(,)"              $ testWriter (runL pure)
  , testGroup "WriterT (Lazy)"   $ testWriter (runL (fmap swap . LazyWriterT.runWriterT))
  , testGroup "WriterT (Strict)" $ testWriter (runL (fmap swap . StrictWriterT.runWriterT))
  , testGroup "RWST (Lazy)"      $ testWriter (runL (runRWST LazyRWST.runRWST))
  , testGroup "RWST (Strict)"    $ testWriter (runL (runRWST StrictRWST.runRWST))
  ] where
  testMonad    run = Monad.test    (m (gen0 w) (genN w b)) a b c initial run
  testMonadFix run = MonadFix.test (m (gen0 w) (genN w b)) a b   initial run
  testWriter   run = Writer.test w (m (gen0 w) (genN w b)) a     initial run
  initial = identity <*> unit
  runRWST f m = (\ (a, _, w) -> (w, a)) <$> f m () ()


gen0 :: Has (Writer w) sig m => GenTerm w -> GenTerm a -> [GenTerm (m a)]
gen0 w a = [ infixL 4 "<$" (<$) <*> a <*> (label "tell" tell <*> w) ]

genN
  :: forall w b m a sig
  .  (Has (Writer w) sig m, Arg b, Arg w, Show b, Show w, Vary b, Vary w)
  => GenTerm w
  -> GenTerm b
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN w b m a =
  [ atom "fmap" fmap <*> fn a <*> (label "listen" (listen @w) <*> m b)
  , subtermM (m a) (label "censor" censor <*> fn w <*>)
  ]


test
  :: (Has (Writer w) sig m, Arg w, Eq a, Eq w, Monoid w, Show a, Show w, Vary w, Functor f)
  => GenTerm w
  -> GenM m
  -> GenTerm a
  -> GenTerm (f ())
  -> Run f ((,) w) m
  -> [TestTree]
test w m a i (Run runWriter) =
  [ testProperty "tell appends a value to the log" . forall (i :. w :. m a :. Nil) $
    \ i w m -> runWriter ((tell w >> m) <$ i) === fmap (first (mappend w)) (runWriter (m <$ i))
  , testProperty "listen eavesdrops on written output" . forall (i :. m a :. Nil) $
    \ i m -> runWriter (listen m <$ i) === fmap (fst &&& id) (runWriter (m <$ i))
  , testProperty "censor revises written output" . forall (i :. fn w :. m a :. Nil) $
    \ i f m -> runWriter (censor f m <$ i) === fmap (first f) (runWriter (m <$ i))
  ]
