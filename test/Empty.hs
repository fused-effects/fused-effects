{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Empty
( tests
, gen0
, test
) where

import qualified Control.Carrier.Empty.Maybe as EmptyC
import Control.Effect.Empty
import Data.Maybe (maybeToList)
import Gen
import qualified Monad
import qualified MonadFix
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC" $
    [ testMonad
    , testMonadFix
    , testEmpty
    ] >>= ($ runL (fmap maybeToList . EmptyC.runEmpty))
  , testGroup "Maybe"  $ testEmpty (runL (pure . maybeToList))
  ] where
  testMonad    run = Monad.test    (m (\ _ -> gen0) (\ _ _ -> [])) a b c (identity <*> unit) run
  testMonadFix run = MonadFix.test (m (\ _ -> gen0) (\ _ _ -> [])) a b   (identity <*> unit) run
  testEmpty    run = Empty.test    (m (\ _ -> gen0) (\ _ _ -> [])) a b   (identity <*> unit) run


gen0 :: Has Empty sig m => [Gen (m a)]
gen0 = [ label "empty" empty ]


test
  :: forall a b m f sig
  .  (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> Gen a
  -> Gen b
  -> Gen (f ())
  -> Run f [] m
  -> [TestTree]
test (GenM m) _ b i (Run runEmpty) =
  [ testProperty "empty annihilates >>=" . forall (i :. fn @a (m b) :. Nil) $
    \ i k -> runEmpty ((empty >>= k) <$ i) === runEmpty (empty <$ i)
  ]
