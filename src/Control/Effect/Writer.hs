{-# LANGUAGE DeriveFunctor, ExplicitForAll, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer
( Writer(..)
, tell
, runWriter
, execWriter
, WriterC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Sum
import Control.Effect.Internal
import Data.Coerce

data Writer w (m :: * -> *) k = Tell w k
  deriving (Functor)

instance HFunctor (Writer w) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Writer w) where
  handle state handler (Tell w k) = Tell w (handler (k <$ state))
  {-# INLINE handle #-}

-- | Write a value to the log.
--
--   prop> fst (run (runWriter (mapM_ (tell . Sum) (0 : ws)))) == foldMap Sum ws
tell :: (Member (Writer w) sig, Carrier sig m) => w -> m ()
tell w = send (Tell w (ret ()))
{-# INLINE tell #-}


-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log alongside the result value.
--
--   prop> run (runWriter (tell (Sum a) *> pure b)) == (Sum a, b)
runWriter :: forall w sig m a . (Carrier sig m, Effect sig, Monoid w) => Eff (WriterC w m) a -> m (w, a)
runWriter m = runWriterC (interpret m) mempty
{-# INLINE runWriter #-}

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log and discarding the result value.
--
--   prop> run (execWriter (tell (Sum a) *> pure b)) == Sum a
execWriter :: forall w sig m a . (Carrier sig m, Effect sig, Functor m, Monoid w) => Eff (WriterC w m) a -> m w
execWriter = fmap fst . runWriter
{-# INLINE execWriter #-}


-- | A space-efficient carrier for 'Writer' effects.
--
--   This is based on a post Gabriel Gonzalez made to the Haskell mailing list: https://mail.haskell.org/pipermail/libraries/2013-March/019528.html
--
--   Note that currently, the constant-space behaviour observed there only occurs when using 'WriterC' and 'VoidC' without 'Eff' wrapping them. See the @benchmark@ component for details.
newtype WriterC w m a = WriterC { runWriterC :: w -> m (w, a) }

instance Functor m => Functor (WriterC w m) where
  fmap f (WriterC run) = WriterC (\ w -> fmap (fmap f) (run w))
  {-# INLINE fmap #-}

instance (Monad m, Monoid w) => Applicative (WriterC w m) where
  pure a = WriterC $ \w -> pure (w, a)
  {-# INLINE pure #-}

  WriterC f <*> WriterC a = WriterC $ \ w -> do
    (w', f') <- f w
    (w'', a') <- a w'
    let fa = f' a'
    fa `seq` pure (w'', fa)
  {-# INLINE (<*>) #-}

instance (Monad m, Monoid w) => Monad (WriterC w m) where
  return = pure
  {-# INLINE return #-}

  m >>= f  = WriterC $ \w -> do
    (w', a) <- runWriterC m w
    runWriterC (f a) w'
  {-# INLINE (>>=) #-}

instance (Monoid w, Carrier sig m, Effect sig) => Carrier (Writer w :+: sig) (WriterC w m) where
  ret a = WriterC (\ w -> ret (w, a))
  {-# INLINE ret #-}

  eff op = WriterC (\ w -> handleSum
    (eff . handleState w runWriterC)
    (\ (Tell w' k) -> let w'' = mappend w w' in w'' `seq` runWriterC k w'')
    op)
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Monoid (Sum(..))
