{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.List where

import Control.Applicative (liftA2)
import Control.Carrier

newtype ListH m a = ListH { runListH :: m [a] }
  deriving (Functor)

instance Applicative m => Applicative (ListH m) where
  pure a = ListH (pure [a])

  ListH f <*> ListH a = ListH (liftA2 (<*>) f a)

instance Monad m => Monad (ListH m) where
  return = pure

  ListH a >>= f = ListH (a >>= fmap concat . traverse (runListH . f))

instance Carrier [] ListH where
  joinl mf = ListH (mf >>= runListH)

  suspend = ListH (pure [[()]])

  resume = fmap concat . traverse runListH

  wrap = ListH
