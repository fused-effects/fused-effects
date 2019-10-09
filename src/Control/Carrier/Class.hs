{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Class
( Carrier(..)
) where

import {-# SOURCE #-} Control.Effect.Choose (Choose(..))
import Control.Effect.Class
import {-# SOURCE #-} Control.Effect.Empty (Empty(..))
import {-# SOURCE #-} Control.Effect.Error (Error(..))
import {-# SOURCE #-} Control.Effect.NonDet (NonDet)
import {-# SOURCE #-} Control.Effect.Reader (Reader(..))
import {-# SOURCE #-} Control.Effect.State (State(..))
import Control.Effect.Sum ((:+:)(..))
import {-# SOURCE #-} Control.Effect.Writer (Writer(..))
import Control.Monad ((<=<))
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import Data.Tuple (swap)

-- | The class of carriers (results) for algebras (effect handlers) over signatures (effects), whose actions are given by the 'eff' method.
class (HFunctor sig, Monad m) => Carrier sig m | m -> sig where
  -- | Construct a value in the carrier for an effect signature (typically a sum of a handled effect and any remaining effects).
  eff :: sig m a -> m a


instance Carrier Choose NonEmpty where
  eff (Choose m) = m True S.<> m False

instance Carrier Empty Maybe where
  eff Empty = Nothing

instance Carrier (Error e) (Either e) where
  eff (Throw e)     = Left e
  eff (Catch m h k) = either (k <=< h) k m

instance Carrier (Reader r) ((->) r) where
  eff (Ask       k) r = k r r
  eff (Local f m k) r = k (m (f r)) r

instance Carrier NonDet [] where
  eff (L Empty)      = []
  eff (R (Choose k)) = k True ++ k False

instance Monoid w => Carrier (Writer w) ((,) w) where
  eff (Tell w (w', k))    = (mappend w w', k)
  eff (Listen m k)        = uncurry k m
  eff (Censor f (w, a) k) = let (w', a') = k a in (mappend (f w) w', a')


-- transformers

instance (Carrier sig m, Effect sig) => Carrier (Error e :+: sig) (Except.ExceptT e m) where
  eff (L (Throw e))     = Except.ExceptT $ pure (Left e)
  eff (L (Catch m h k)) = Except.ExceptT $ Except.runExceptT m >>= either (either (pure . Left) (Except.runExceptT . k) <=< Except.runExceptT . h) (Except.runExceptT . k)
  eff (R other)         = Except.ExceptT $ eff (handle (Right ()) (either (pure . Left) Except.runExceptT) other)

instance Carrier sig m => Carrier (Reader r :+: sig) (Reader.ReaderT r m) where
  eff (L (Ask       k)) = Reader.ask >>= k
  eff (L (Local f m k)) = Reader.local f m >>= k
  eff (R other)         = Reader.ReaderT $ \ r -> eff (hmap (flip Reader.runReaderT r) other)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (State.Strict.StateT s m) where
  eff (L (Get   k)) = State.Strict.get >>= k
  eff (L (Put s k)) = State.Strict.put s *> k
  eff (R other)     = State.Strict.StateT $ \ s -> swap <$> eff (handle (s, ()) (\ (s, x) -> swap <$> State.Strict.runStateT x s) other)

instance (Carrier sig m, Effect sig) => Carrier (State s :+: sig) (State.Lazy.StateT s m) where
  eff (L (Get   k)) = State.Lazy.get >>= k
  eff (L (Put s k)) = State.Lazy.put s *> k
  eff (R other)     = State.Lazy.StateT $ \ s -> swap <$> eff (handle (s, ()) (\ (s, x) -> swap <$> State.Lazy.runStateT x s) other)

instance (Carrier sig m, Effect sig, Monoid w) => Carrier (Writer w :+: sig) (Writer.CPS.WriterT w m) where
  eff (L (Tell w k))     = Writer.CPS.tell w *> k
  eff (L (Listen m k))   = Writer.CPS.listen m >>= uncurry (flip k)
  eff (L (Censor f m k)) = Writer.CPS.censor f m >>= k
  eff (R other)          = Writer.CPS.writerT $ swap <$> eff (handle (mempty, ()) (\ (s, x) -> swap . fmap (mappend s) <$> Writer.CPS.runWriterT x) other)
