{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.Carrier.Identity where

import Control.Carrier
import Data.Functor.Identity

newtype IdentityH m a = IdentityH { runIdentityH :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier IdentityH Identity where
  joinl mf = IdentityH (mf >>= runIdentityH)

  suspend = IdentityH (pure (Identity ()))

  resume = fmap Identity . runIdentityH . runIdentity

  wrap = IdentityH . fmap runIdentity
