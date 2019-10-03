{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Catch
( Catch(..)
) where

data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)
