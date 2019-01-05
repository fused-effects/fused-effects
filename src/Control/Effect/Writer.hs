{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
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
import Data.Bifunctor (first)
import Data.Coerce

data Writer w (m :: * -> *) k = Tell w k
  deriving (Functor)

instance HFunctor (Writer w) where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect (Writer w) where
  handle state handler (Tell w k) = Tell w (handler (k <$ state))

-- | Write a value to the log.
--
--   prop> fst (run (runWriter (mapM_ (tell . Sum) (0 : ws)))) == foldMap Sum ws
tell :: (Member (Writer w) sig, Carrier sig m) => w -> m ()
tell w = send (Tell w (ret ()))


-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log alongside the result value.
--
--   prop> run (runWriter (tell (Sum a) *> pure b)) == (Sum a, b)
runWriter :: (Carrier sig m, Effect sig, Functor m, Monoid w) => Eff (WriterC w m) a -> m (w, a)
runWriter m = runWriterC (interpret m)

-- | Run a 'Writer' effect with a 'Monoid'al log, producing the final log and discarding the result value.
--
--   prop> run (execWriter (tell (Sum a) *> pure b)) == Sum a
execWriter :: (Carrier sig m, Effect sig, Functor m, Monoid w) => Eff (WriterC w m) a -> m w
execWriter = fmap fst . runWriter


newtype WriterC w m a = WriterC { runWriterC :: m (w, a) }

instance (Monoid w, Carrier sig m, Effect sig, Functor m) => Carrier (Writer w :+: sig) (WriterC w m) where
  ret a = WriterC (ret (mempty, a))
  eff = WriterC . handleSum
    (eff . handle (mempty, ()) (uncurry runWriter'))
    (\ (Tell w k) -> first (mappend w) <$> runWriterC k)
    where runWriter' w = fmap (first (mappend w)) . runWriterC


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Data.Monoid (Sum(..))
