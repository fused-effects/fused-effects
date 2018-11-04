{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cull where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.NonDet
import Control.Effect.Sum

data Cull m k
  = forall a . Cull (m a) (a -> k)

deriving instance Functor (Cull m)

instance HFunctor Cull where
  hmap f (Cull m k) = Cull (f m) k
  {-# INLINE hmap #-}

instance Effect Cull where
  handle state handler (Cull m k) = Cull (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

cull :: (Carrier sig m, Member Cull sig) => m a -> m a
cull m = send (Cull m ret)


runCull :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (CullC m) a -> m a
runCull = (>>= maybe empty pure) . runCullC . interpret

newtype CullC m a = CullC { runCullC :: m (Maybe a) }

instance (Carrier sig m, Effect sig, Monad m) => Carrier (Cull :+: NonDet :+: sig) (CullC m) where
  ret = CullC . ret . Just
  {-# INLINE ret #-}

  eff = CullC . handleSum (handleSum
    (eff . handle (Just ()) (maybe (ret Nothing) runCullC))
    (\case
      Empty       -> ret Nothing
      Choose k    -> runCullC (k True) >>= maybe (runCullC (k False)) (ret . Just)))
    (\ (Cull m k) -> runCullC m >>= maybe (ret Nothing) (runCullC . k))
