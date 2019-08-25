{-# LANGUAGE DeriveTraversable #-}
module Data.BinaryTree
(-- * Binary trees
  Bin(..)
, foldBin
) where

import Control.Applicative (Alternative (..))

data Bin a = Nil | Leaf a | Fork (Bin a) (Bin a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative Bin where
  pure = Leaf
  Nil      <*> _ = Nil
  Leaf f   <*> a = fmap f a
  Fork a b <*> c = Fork (a <*> c) (b <*> c)

instance Alternative Bin where
  empty = Nil
  (<|>) = Fork

instance Monad Bin where
  Nil    >>= _   = Nil
  Leaf a >>= f   = f a
  Fork a b >>= f = Fork (a >>= f) (b >>= f)

foldBin :: (b -> b -> b) -> (a -> b) -> b -> Bin a -> b
foldBin fork leaf nil = go where
  go Nil = nil
  go (Leaf a) = leaf a
  go (Fork a b) = fork (go a) (go b)
