{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Choose
( tests
, genN
, test
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import           Control.Effect.Choose
import           Data.List.NonEmpty
import           Gen
import qualified Monad
import qualified MonadFix

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC"  $
    [ testMonad
    , testMonadFix
    , testChoose
    ] >>= ($ runL (ChooseC.runChooseS (pure . pure)))
  , testGroup "NonEmpty" $ testChoose (runL (pure . toList))
  ] where
  testMonad    run = Monad.test    (genM mempty genN) termA termB termC initial run
  testMonadFix run = MonadFix.test (genM mempty genN) termA termB       initial run
  testChoose   run = Choose.test   (genM mempty genN) termA termB       initial run
  initial = identity <*> unit


genN :: Has Choose sig m => GenM m -> GenTerm a -> [GenTerm (m a)]
genN m a = [ addLabel "<|>" (subtermM2 (m a) (m a) (\ a b -> infixL 3 "<|>" (<|>) <*> a <*> b)) ]


test
  :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a, Functor f)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f [] m
  -> [TestTree]
test m a b i (Run runChoose) =
  [ testProperty ">>= distributes over <|>" . forall_ (i :. m a :. m a :. fn (m b) :. Nil) $
    \ i m n k -> runChoose (((m <|> n) >>= k) <$ i) === runChoose (((m >>= k) <|> (n >>= k)) <$ i)
  , testProperty "<|> is associative" . forall_ (i :. m a :. m a :. m a :. Nil) $
    \ i m n o -> runChoose (((m <|> n) <|> o) <$ i) === runChoose ((m <|> (n <|> o)) <$ i)
  ]
