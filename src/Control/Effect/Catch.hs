{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Catch
( Catch(..)
) where

import Control.Carrier

data Catch e m k
  = forall b . Catch (m b) (e -> m b) (b -> m k)

deriving instance Functor m => Functor (Catch e m)

instance HFunctor (Catch e) where
  hmap f (Catch m h k) = Catch (f m) (f . h) (f . k)
