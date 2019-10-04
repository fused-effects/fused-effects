{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.NonDet.Maybe
( -- * NonDet effects
  module Control.Effect.NonDet
  -- * NonDet carrier
, runNonDet
, NonDetC(..)
  -- * Re-exports
, Carrier
, run
) where

import Control.Carrier
import Control.Effect.NonDet
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- | Run 'NonDet' effects, returning 'Nothing' for empty computations, or 'Just' the result otherwise. Note that this will terminate on the first successful result.
--
--   prop> run (runNonDet empty)    === Nothing
--   prop> run (runNonDet (pure a)) === Just a
--   prop> run (runNonDet (let f x = pure x <|> f x in f a)) === Just a
runNonDet :: NonDetC m a -> m (Maybe a)
runNonDet = runMaybeT . runNonDetC
{-# INLINE runNonDet #-}

newtype NonDetC m a = NonDetC { runNonDetC :: MaybeT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

-- | 'NonDetC' passes 'Fail.MonadFail' operations along to the underlying monad @m@, rather than interpreting it as a synonym for 'empty' à la 'MaybeT'.
instance Fail.MonadFail m => Fail.MonadFail (NonDetC m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance (Carrier sig m, Effect sig) => Carrier (NonDet :+: sig) (NonDetC m) where
  eff (L (L Empty))      = empty
  eff (L (R (Choose k))) = k True <|> k False
  eff (R other) = NonDetC (MaybeT (eff (handle (Just ()) (maybe (pure Nothing) runNonDet) other)))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
