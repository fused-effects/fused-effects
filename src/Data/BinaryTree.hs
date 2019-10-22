{-# LANGUAGE DeriveTraversable #-}
module Data.BinaryTree
( BinaryTree(..)
, fold
) where

import Control.Applicative (Alternative(..))

data BinaryTree a = Nil | Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  f <*> a = fold Fork (<$> a) Nil f
  {-# INLINE (<*>) #-}

instance Alternative BinaryTree where
  empty = Nil
  {-# INLINE empty #-}
  (<|>) = Fork
  {-# INLINE (<|>) #-}

instance Monad BinaryTree where
  a >>= f = fold Fork f Nil a
  {-# INLINE (>>=) #-}


fold :: (b -> b -> b) -> (a -> b) -> b -> BinaryTree a -> b
fold fork leaf nil = go where
  go Nil        = nil
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
{-# INLINE fold #-}
