{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Control.Effect.Once where

import Control.Effect.Carrier
import Control.Effect.Sum

data Once m k
  = forall a . Once (m a) (a -> k)

deriving instance Functor (Once m)

instance HFunctor Once where
  hmap f (Once m k) = Once (f m) k
  {-# INLINE hmap #-}

instance Effect Once where
  handle state handler (Once m k) = Once (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

once :: (Carrier sig m, Member Once sig) => m a -> m a
once m = send (Once m ret)
