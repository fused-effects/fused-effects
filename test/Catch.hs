{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Catch
( genN
, test
) where

import Control.Effect.Error
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

genN
  :: forall e m a sig
  .  (Has (Catch e) sig m, Arg e, Show e, Vary e)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> [GenTerm (m a)]
genN _ m a = [ addLabel "catchError" $ subtermM (m a) (\ m' -> infixL 9 "`catchError`" catchError <*> m' <*> fn @e (m a)) ]


test
  :: (Has (Error e) sig m, Arg e, Eq a, Eq e, Show a, Show e, Vary e, Functor f)
  => GenTerm e
  -> GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f (Either e) m
  -> [TestTree]
test e m a _ i (Run runCatch) =
  [ testProperty "catchError intercepts throwError" . forall (i :. e :. fn (m a) :. Nil) $
    \ i e h -> runCatch ((throwError e `catchError` h) <$ i) === runCatch (h e <$ i)
  ]
