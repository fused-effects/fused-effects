{-# LANGUAGE DeriveTraversable #-}
module Data.BinaryTree.NonEmpty
( BinaryTree(..)
, fold
) where

data BinaryTree a = Leaf a | Fork (BinaryTree a) (BinaryTree a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative BinaryTree where
  pure = Leaf
  {-# INLINE pure #-}
  f <*> a = fold Fork (<$> a) f
  {-# INLINE (<*>) #-}

instance Monad BinaryTree where
  a >>= f = fold Fork f a
  {-# INLINE (>>=) #-}


fold :: (b -> b -> b) -> (a -> b) -> BinaryTree a -> b
fold fork leaf = go where
  go (Leaf a)   = leaf a
  go (Fork a b) = fork (go a) (go b)
{-# INLINE fold #-}
