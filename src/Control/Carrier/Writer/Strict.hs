{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}

{- | A carrier for 'Writer' effects. This carrier performs its append operations strictly and thus avoids the space leaks inherent in lazy writer monads.

This implementation is based on a post Gabriel Gonzalez made to the Haskell mailing list: <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html>
-}

module Control.Carrier.Writer.Strict
( -- * Writer carrier
  runWriter
, execWriter
, WriterC(..)
  -- * Writer effect
, module X
) where

import Control.Applicative (Alternative(..))
import Control.Carrier
import Control.Carrier.State.Strict
import Control.Effect.Writer
import Control.Effect.Writer as X (Writer)
import Control.Effect.Writer as X hiding (Writer)
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log alongside the result value.
--
-- @
-- 'runWriter' ('tell' w) = 'pure' (w, ())
-- @
-- @
-- 'runWriter' ('pure' a) = 'pure' ('mempty', a)
-- @
runWriter :: Monoid w => WriterC w m a -> m (w, a)
runWriter (WriterC m) = runState mempty m
{-# INLINE runWriter #-}

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log and discarding the result value.
--
-- @
-- 'execWriter' m = 'fmap' 'fst' ('runWriter' m)
-- @
execWriter :: (Monoid w, Functor m) => WriterC w m a -> m w
execWriter = fmap fst . runWriter
{-# INLINE execWriter #-}


-- | A space-efficient carrier for 'Writer' effects, implemented atop "Control.Carrier.State.Strict".
newtype WriterC w m a = WriterC { runWriterC :: StateC w m a }
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

instance (Monoid w, Carrier sig m, Effect sig) => Carrier (Writer w :+: sig) (WriterC w m) where
  eff (L (Tell w     k)) = WriterC $ do
    modify (`mappend` w)
    runWriterC k
  eff (L (Listen   m k)) = WriterC $ do
    w <- get
    put (mempty :: w)
    a <- runWriterC m
    w' <- get
    modify (mappend (w :: w))
    runWriterC (k w' a)
  eff (L (Censor f m k)) = WriterC $ do
    w <- get
    put (mempty :: w)
    a <- runWriterC m
    modify (mappend w . f)
    runWriterC (k a)
  eff (R other)          = WriterC (eff (R (handleCoercible other)))
  {-# INLINE eff #-}
