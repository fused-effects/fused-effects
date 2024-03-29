{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- | An effect modelling nondeterminism without failure (one or more successful results).

The 'Control.Effect.NonDet.NonDet' effect is the composition of 'Choose' and 'Empty'.

Predefined carriers:

* "Control.Carrier.Choose.Church".
* If 'Choose' is the last effect in a stack, it can be interpreted directly to a 'NonEmpty'.

@since 1.0.0.0
-}

module Control.Effect.Choose
( -- * Choose effect
  Choose(..)
, (<|>)
, optional
, many
, some
, some1
  -- * Choosing semigroup
, Choosing(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Algebra
import qualified Control.Applicative as A
import           Control.Effect.Choose.Internal (Choose(..))
import           Control.Effect.Empty
import           Control.Monad (MonadPlus)
import           Control.Monad.Fail as Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Zip
import           Data.Bool (bool)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Semigroup as S

-- | Nondeterministically choose between two computations.
--
-- @
-- (m '<|>' n) '>>=' k = (m '>>=' k) '<|>' (n '>>=' k)
-- @
-- @
-- (m '<|>' n) '<|>' o = m '<|>' (n '<|>' o)
-- @
-- @
-- 'empty' '<|>' m = m
-- @
-- @
-- m '<|>' 'empty' = m
-- @
--
-- @since 1.0.0.0
(<|>) :: Has Choose sig m => m a -> m a -> m a
a <|> b = send Choose >>= bool b a
{-# INLINE (<|>) #-}

infixl 3 <|>

-- | Select between 'Just' the result of an operation, and 'Nothing'.
--
-- @
-- 'optional' 'empty' = 'pure' 'Nothing'
-- @
-- @
-- 'optional' ('pure' a) = 'pure' ('Just' a)
-- @
--
-- @since 1.0.0.0
optional :: Has Choose sig m => m a -> m (Maybe a)
optional a = Just <$> a <|> pure Nothing
{-# INLINE optional #-}

-- | Zero or more.
--
-- @
-- 'many' m = 'some' m '<|>' 'pure' []
-- @
--
-- @since 1.0.0.0
many :: Has Choose sig m => m a -> m [a]
many a = go where go = (:) <$> a <*> go <|> pure []
{-# INLINE many #-}

-- | One or more.
--
-- @
-- 'some' m = (:) '<$>' m '<*>' 'many' m
-- @
--
-- @since 1.0.0.0
some :: Has Choose sig m => m a -> m [a]
some a = (:) <$> a <*> many a
{-# INLINE some #-}

-- | One or more, returning a 'NonEmpty' list of the results.
--
-- @
-- 'some1' m = (':|') '<$>' m '<*>' 'many' m
-- @
--
-- @since 1.0.0.0
some1 :: Has Choose sig m => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a
{-# INLINE some1 #-}


-- | @since 1.0.0.0
newtype Choosing m a = Choosing { getChoosing :: m a }
  deriving (Algebra sig, Applicative, Foldable, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadUnliftIO, MonadZip)

instance Has Choose sig m => S.Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (m1 <|> m2)
  {-# INLINE (<>) #-}

instance (Has Choose sig m, Has Empty sig m) => Monoid (Choosing m a) where
  mempty = Choosing empty
  {-# INLINE mempty #-}

  mappend = (S.<>)
  {-# INLINE mappend #-}

instance (Has Choose sig m, Has Empty sig m) => A.Alternative (Choosing m) where
  empty = mempty
  {-# INLINE empty #-}

  (<|>) = mappend
  {-# INLINE (<|>) #-}

instance (Has Choose sig m, Has Empty sig m) => MonadPlus (Choosing m)

instance MonadTrans Choosing where
  lift = Choosing
  {-# INLINE lift #-}

instance Traversable m => Traversable (Choosing m) where
  sequenceA (Choosing m) = fmap Choosing (sequenceA m)
  {-# INLINE sequenceA #-}

  traverse f (Choosing m) = fmap Choosing (traverse f m)
  {-# INLINE traverse #-}

  sequence (Choosing m) = fmap Choosing (sequence m)
  {-# INLINE sequence #-}

  mapM f (Choosing m) = fmap Choosing (mapM f m)
  {-# INLINE mapM #-}
