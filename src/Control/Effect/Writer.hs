{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Control.Effect.Writer
( Writer(..)
, tell
, runWriter
, WriterH(..)
) where

import Control.Effect.Handler
import Control.Effect.Sum
import Control.Effect.Internal
import Data.Bifunctor (first)

data Writer w m k = Tell w k
  deriving (Functor)

instance HFunctor (Writer w) where
  hfmap _ (Tell w k) = Tell w k

instance Effect (Writer w) where
  handle state handler (Tell w k) = Tell w (handler (k <$ state))

-- | Write a value to the log.
--
--   prop> fst (run (runWriter (replicateM (abs n) (tell (Sum w))))) == stimes (abs n) (Sum w)
tell :: (Member (Writer w) sig, Carrier sig m) => w -> m ()
tell w = send (Tell w (gen ()))


runWriter :: (Effectful sig m, Monoid w) => Eff (WriterH w m) a -> m (w, a)
runWriter m = runWriterH (interpret m)

newtype WriterH w m a = WriterH { runWriterH :: m (w, a) }

instance (Monoid w, Effectful sig m) => Carrier (Writer w :+: sig) (WriterH w m) where
  gen a = WriterH (pure (mempty, a))
  alg = algW \/ (WriterH . alg . handle (mempty, ()) (uncurry runWriter'))
    where algW (Tell w k) = WriterH (first (w <>) <$> runWriterH k)
          runWriter' w = fmap (first (w <>)) . runWriterH


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
-- >>> import Control.Monad (replicateM)
-- >>> import Data.Monoid (Sum(..))
-- >>> import Data.Semigroup (stimes)
