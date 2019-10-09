{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MonoLocalBinds, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
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
, Has
) where

import {-# SOURCE #-} Control.Carrier
import Control.Effect.Empty
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Semigroup as S
import GHC.Generics (Generic1)

newtype Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
(<|>) :: Has Choose sig m => m a -> m a -> m a
(<|>) a b = send (Choose (bool b a))

infixl 3 <|>

-- | Select between 'Just' the result of an operation, and 'Nothing'.
optional :: Has Choose sig m => m a -> m (Maybe a)
optional a = Just <$> a <|> pure Nothing

-- | Zero or more.
many :: Has Choose sig m => m a -> m [a]
many a = go where go = (:) <$> a <*> go <|> pure []

-- | One or more.
some :: Has Choose sig m => m a -> m [a]
some a = (:) <$> a <*> many a

-- | One or more, returning a 'NonEmpty' list of the results.
some1 :: Has Choose sig m => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a


newtype Choosing m a = Choosing { getChoosing :: m a }

instance Has Choose sig m => S.Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (m1 <|> m2)

instance (Has Choose sig m, Has Empty sig m) => Monoid (Choosing m a) where
  mempty = Choosing empty
  mappend = (S.<>)
