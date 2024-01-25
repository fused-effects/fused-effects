{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Accum
( tests
) where

import qualified Control.Carrier.Accum.Church as C.Accum.Church
import qualified Control.Carrier.Accum.Strict as C.Accum.Strict
import           Control.Effect.Accum
#if MIN_VERSION_transformers(0,5,4)
import qualified Control.Monad.Trans.Accum as T.Accum
import           Data.Tuple (swap)
#endif
import           Gen
import qualified Monad
import qualified MonadFix
import Data.Bifunctor (first)

tests :: TestTree
tests = testGroup "Accum"
  [ testGroup "AccumC (Church)" $
    [ testMonad
    , testMonadFix
    , testAccum
    ] >>= ($ runC (C.Accum.Church.runAccum (curry pure)))
  , testGroup "AccumC (Strict)" $
    [ testMonad
    , testMonadFix
    , testAccum
    ] >>= ($ runC C.Accum.Strict.runAccum)
#if MIN_VERSION_transformers(0,5,4)
  , testGroup "AccumT" $ testAccum (runC (fmap (fmap swap) . flip T.Accum.runAccumT))
#endif
  ] where
  testMonad    run = Monad.test    (genM (gen0 termW) (\_ _ -> [])) termA termB termC initial run
  testMonadFix run = MonadFix.test (genM (gen0 termW) (\_ _ -> [])) termA termB       initial run
  testAccum    run = Accum.test    (genM (gen0 termW) (\_ _ -> [])) termA             termW   run
  initial = pair <*> termW <*> unit

gen0
  :: forall w sig m a
  .  (Has (Accum w) sig m, Arg w, Vary w, Show w)
  => GenTerm w
  -> GenTerm a
  -> [GenTerm (m a)]
gen0 w a =
  [ infixL 4 "<$" (<$) <*> a <*> (label "add" add <*> w)
  , label "looks" (looks @w) <*> fn a
  ]

test
  :: forall w sig m a
  .  (Has (Accum w) sig m, Arg w, Eq a, Eq w, Show a, Show w, Vary w, Monoid w)
  => GenM m
  -> GenTerm a
  -> GenTerm w
  -> Run ((,) w) ((,) w) m
  -> [TestTree]
test m a w (Run runAccum) =
  [ testProperty "look returns the log variable (simple)" . forall_ (w :. Nil) $
    \ w -> runAccum (w, look) === Identity (mempty, w)
  , testProperty "add appends to the log variable (simple)" . forall_ (w :. w :. Nil) $
    \ w0 w -> runAccum (w0, add w) === Identity (w, ())
  , testProperty "look returns the log variable (continuation)" . forall_ (w :. fn (m a) :. Nil) $
    \ w0 k -> runAccum (w0, look >>= k) === runAccum (w0, k w0)
  , testProperty "add appends to the log variable and alters the environment for look" . forall_ (w :. w :. Nil) $
    \ w0 w -> runAccum (w0, add w >> look) === runAccum (mappend w0 w, look @w <* add w)
  , testProperty "add appends to the log variable and alters the environment for continuations" . forall_ (w :. w :. m a :. Nil) $
    \ w0 w k -> runAccum (w0, add w >> k) === (first (mappend w) <$> runAccum (mappend w0 w, k))
  ]
