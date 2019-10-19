{-# LANGUAGE RankNTypes #-}
module Choose
( tests
, gen
, test
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty
import Gen
import qualified Monad
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ test "ChooseC"  (ChooseC.runChooseS (pure . pure))
  , test "NonEmpty" (pure . toList)
  ] where
  test :: Has Choose sig m => String -> (forall a . m a -> PureC [a]) -> TestTree
  test name run = testGroup name
    $  Monad.test  (m gen) a b c (pure (Identity ())) (run . runIdentity)
    ++ Choose.test (m gen) a b                        run


gen
  :: Has Choose sig m
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = addLabel "<|>" (infixL 3 "<|>" (<|>) <*> m a <*> m a)


test
  :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Gen a -> Gen (m a))
  -> Gen a
  -> Gen b
  -> (forall a . m a -> PureC [a])
  -> [TestTree]
test m a b runChoose =
  [ testProperty ">>= distributes over <|>" . forall (m a :. m a :. fn (m b) :. Nil) $
    \ m n k -> runChoose ((m <|> n) >>= k) === runChoose ((m >>= k) <|> (n >>= k))
  , testProperty "<|> is associative" . forall (m a :. m a :. m a :. Nil) $
    \ m n o -> runChoose ((m <|> n) <|> o) === runChoose (m <|> (n <|> o))
  ]
