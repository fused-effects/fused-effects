{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Effect.Choose
( -- * Choose effect
  Choose(..)
, choose
) where

import Control.Carrier.Class
import Data.Bool (bool)
import GHC.Generics (Generic1)

data Choose m k
  = Choose (Bool -> m k)
  deriving (Functor, Generic1)

instance HFunctor Choose
instance Effect   Choose

-- | Nondeterministically choose between two computations.
choose :: (Carrier sig m, Member Choose sig) => m a -> m a -> m a
choose a b = send (Choose (bool b a))
