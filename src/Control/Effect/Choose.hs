{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
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
) where

import Control.Carrier
import Control.Effect.Empty
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Semigroup as S
import GHC.Generics (Generic1)

data Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
(<|>) :: (Carrier sig m, Member Choose sig) => m a -> m a -> m a
(<|>) a b = send (Choose (bool b a))

infixl 3 <|>

-- | Select between 'Just' the result of an operation, and 'Nothing'.
optional :: (Carrier sig m, Member Choose sig) => m a -> m (Maybe a)
optional a = Just <$> a <|> pure Nothing

-- | Zero or more.
many :: (Carrier sig m, Member Choose sig) => m a -> m [a]
many a = go where go = (:) <$> a <*> go <|> pure []

-- | One or more.
some :: (Carrier sig m, Member Choose sig) => m a -> m [a]
some a = (:) <$> a <*> many a

-- | One or more, returning a 'NonEmpty' list of the results.
some1 :: (Carrier sig m, Member Choose sig) => m a -> m (NonEmpty a)
some1 a = (:|) <$> a <*> many a


newtype Choosing m a = Choosing { getChoosing :: m a }

instance (Carrier sig m, Member Choose sig) => S.Semigroup (Choosing m a) where
  Choosing m1 <> Choosing m2 = Choosing (m1 <|> m2)

instance (Carrier sig m, Member Choose sig, Member Empty sig) => Monoid (Choosing m a) where
  mempty = Choosing empty
  mappend = (S.<>)
