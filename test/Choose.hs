{-# LANGUAGE RankNTypes #-}
module Choose
( tests
, gen
, test
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Data.List.NonEmpty
import Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC"  $ test (m gen) a b (ChooseC.runChooseS (pure . pure))
  , testGroup "NonEmpty" $ test (m gen) a b (pure . toList)
  ]


gen
  :: (Has Choose sig m, Show a)
  => (forall a . Show a => Gen a -> Gen (m a))
  -> Gen a
  -> Gen (m a)
gen m a = addLabel "<|>" (infixL 3 "<|>" (<|>) <*> m a <*> m a)


test
  :: (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a)
  => (forall a . Show a => Gen a -> Gen (m a))
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
