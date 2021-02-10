{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Empty
( tests
, gen0
, test
) where

import qualified Control.Carrier.Empty.Church as C.Church
import qualified Control.Carrier.Empty.Maybe as C.Maybe
import qualified Control.Monad.Trans.Maybe as T.Maybe
import           Control.Effect.Empty
import           Data.Maybe (maybeToList)
import           Gen
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "Empty"
  [ testGroup "EmptyC (Church)" $
    [ testMonad
    , testMonadFix
    , testEmpty
    ] >>= ($ runL (fmap maybeToList . C.Church.runEmpty (pure Nothing) (pure . Just)))
  , testGroup "EmptyC (Maybe)" $
    [ testMonad
    , testMonadFix
    , testEmpty
    ] >>= ($ runL (fmap maybeToList . C.Maybe.runEmpty))
  , testGroup "MaybeT" $ testEmpty (runL (fmap maybeToList . T.Maybe.runMaybeT))
  , testGroup "Maybe"  $ testEmpty (runL (pure . maybeToList))
  ] where
  testMonad    run = Monad.test    (m gen0 (\ _ _ -> [])) a b c initial run
  testMonadFix run = MonadFix.test (m gen0 (\ _ _ -> [])) a b   initial run
  testEmpty    run = Empty.test    (m gen0 (\ _ _ -> [])) a b   initial run
  initial = identity <*> unit


gen0 :: Has Empty sig m => GenTerm a -> [GenTerm (m a)]
gen0 _ = [ label "empty" empty ]


test
  :: forall a b m f sig
  .  (Has Empty sig m, Arg a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f [] m
  -> [TestTree]
test m _ b i (Run runEmpty) =
  [ testProperty "empty annihilates >>=" . forall (i :. fn @a (m b) :. Nil) $
    \ i k -> runEmpty ((empty >>= k) <$ i) === runEmpty (empty <$ i)
  ]
