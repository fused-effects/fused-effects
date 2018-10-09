{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.Carrier.Identity where

import Control.Carrier
import Data.Functor.Identity

newtype IdentityH m a = IdentityH { runIdentityH :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier Identity IdentityH where
  joinl mf = IdentityH (mf >>= runIdentityH)

  suspend f = f (Identity ())

  resume = fmap Identity . runIdentityH . runIdentity

  wrap = IdentityH . fmap runIdentity
