{-# LANGUAGE DeriveTraversable #-}
module Data.BinaryTree
(-- * Binary trees
  BinaryTree(..)
, fold
) where

import Control.Applicative (Alternative (..))

data BinaryTree a = Nil | Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  Nil      <*> _ = Nil
  Leaf f   <*> a = fmap f a
  Fork a b <*> c = Fork (a <*> c) (b <*> c)

instance Alternative BinaryTree where
  empty = Nil
  (<|>) = Fork

instance Monad BinaryTree where
  Nil    >>= _   = Nil
  Leaf a >>= f   = f a
  Fork a b >>= f = Fork (a >>= f) (b >>= f)

fold :: (b -> b -> b) -> (a -> b) -> b -> BinaryTree a -> b
fold fork leaf nil = go where
  go Nil = nil
  go (Leaf a) = leaf a
  go (Fork a b) = fork (go a) (go b)
