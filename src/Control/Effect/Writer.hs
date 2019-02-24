{-# LANGUAGE DeriveFunctor, ExistentialQuantification, ExplicitForAll, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer
( Writer(..)
, tell
, listen
, listens
, censor
, runWriter
, execWriter
, WriterC(..)
) where

import Control.Effect.Carrier
import Control.Effect.Internal
import Control.Effect.State
import Control.Effect.Sum
import Control.Monad.Fail

data Writer w m k
  = Tell w k
  | forall a . Listen (m a) (w -> a -> k)
  | forall a . Censor (w -> w) (m a) (a -> k)

deriving instance Functor (Writer w m)

instance HFunctor (Writer w) where
  hmap _ (Tell w     k) = Tell w         k
  hmap f (Listen   m k) = Listen   (f m) k
  hmap f (Censor g m k) = Censor g (f m) k
  {-# INLINE hmap #-}

instance Effect (Writer w) where
  handle state handler (Tell w     k) = Tell w                          (handler (k <$ state))
  handle state handler (Listen   m k) = Listen   (handler (m <$ state)) (fmap handler . fmap . k)
  handle state handler (Censor f m k) = Censor f (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Write a value to the log.
--
--   prop> fst (run (runWriter (mapM_ (tell . Sum) (0 : ws)))) == foldMap Sum ws
tell :: (Member (Writer w) sig, Carrier sig m) => w -> m ()
tell w = send (Tell w (ret ()))
{-# INLINE tell #-}

-- | Run a computation, returning the pair of its output and its result.
--
--   prop> run (runWriter (fst <$ tell (Sum a) <*> listen @(Sum Integer) (tell (Sum b)))) == (Sum a <> Sum b, Sum b)
listen :: (Member (Writer w) sig, Carrier sig m) => m a -> m (w, a)
listen m = send (Listen m (curry ret))
{-# INLINE listen #-}

-- | Run a computation, applying a function to its output and returning the pair of the modified output and its result.
--
--   prop> run (runWriter (fst <$ tell (Sum a) <*> listens @(Sum Integer) (applyFun f) (tell (Sum b)))) == (Sum a <> Sum b, applyFun f (Sum b))
listens :: (Member (Writer w) sig, Carrier sig m) => (w -> b) -> m a -> m (b, a)
listens f m = send (Listen m (curry ret . f))
{-# INLINE listens #-}

-- | Run a computation, modifying its output with the passed function.
--
--   prop> run (execWriter (censor (applyFun f) (tell (Sum a)))) == applyFun f (Sum a)
--   prop> run (execWriter (tell (Sum a) *> censor (applyFun f) (tell (Sum b)) *> tell (Sum c))) == (Sum a <> applyFun f (Sum b) <> Sum c)
censor :: (Member (Writer w) sig, Carrier sig m) => (w -> w) -> m a -> m a
censor f m = send (Censor f m ret)
{-# INLINE censor #-}


-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log alongside the result value.
--
--   prop> run (runWriter (tell (Sum a) *> pure b)) == (Sum a, b)
runWriter :: forall w sig m a . (Carrier sig m, Effect sig, Monad m, Monoid w) => Eff (WriterC w m) a -> m (w, a)
runWriter m = runStateC (runWriterC (interpret m)) mempty
{-# INLINE runWriter #-}

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log and discarding the result value.
--
--   prop> run (execWriter (tell (Sum a) *> pure b)) == Sum a
execWriter :: forall w sig m a . (Carrier sig m, Effect sig, Monad m, Monoid w) => Eff (WriterC w m) a -> m w
execWriter = fmap fst . runWriter
{-# INLINE execWriter #-}


-- | A space-efficient carrier for 'Writer' effects.
--
--   This is based on a post Gabriel Gonzalez made to the Haskell mailing list: https://mail.haskell.org/pipermail/libraries/2013-March/019528.html
--
--   Note that currently, the constant-space behaviour observed there only occurs when using 'WriterC' and 'VoidC' without 'Eff' wrapping them. See the @benchmark@ component for details.
newtype WriterC w m a = WriterC { runWriterC :: StateC w m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance (Monoid w, Carrier sig m, Effect sig, Monad m) => Carrier (Writer w :+: sig) (WriterC w m) where
  ret = pure
  {-# INLINE ret #-}

  eff op = WriterC (StateC (\ w -> handleSum (eff . handleState w (runStateC . runWriterC)) (\case
    Tell w'    k -> let w'' = mappend w w' in w'' `seq` runStateC (runWriterC k) w''
    Listen   m k -> do
      (w', a) <- runStateC (runWriterC m) mempty
      let w'' = mappend w w'
      w'' `seq` runStateC (runWriterC (k w' a)) w''
    Censor f m k -> do
      (w', a) <- runStateC (runWriterC m) mempty
      let w'' = mappend w (f w')
      w'' `seq` runStateC (runWriterC (k a)) w'')
    op))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Semigroup (Semigroup(..), Sum(..))
