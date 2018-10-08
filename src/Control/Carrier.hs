{-# LANGUAGE FunctionalDependencies #-}
module Control.Carrier where

class Functor f => Carrier c f | c -> f where
  -- | (Left-)join a 'Monad' of 'Carrier's into a 'Carrier'.
  -- @
  -- joinl . pure = id
  -- @
  --
  -- @
  -- joinl . join = joinl . fmap joinl
  -- @
  joinl :: Monad m => m (c m a) -> c m a

  suspend :: Monad m => c m (f ())

  resume :: Monad m => f (c m a) -> m (f a)

  wrap :: Monad m => m (f a) -> c m a
