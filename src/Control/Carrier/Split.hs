{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses #-}
module Control.Carrier.Split where

import Control.Applicative (Alternative(..), liftA2)
import Control.Arrow ((***))
import Control.Carrier

newtype SplitH m a = SplitH { runSplitH :: m (Maybe (a, SplitH m a)) }
  deriving (Functor)

joinSplitH :: Monad m => SplitH m a -> m [a]
joinSplitH = (>>= maybe (pure []) (\ (a, q) -> (a :) <$> joinSplitH q)) . runSplitH

altSplitH :: (Alternative m, Monad m) => SplitH m a -> m a
altSplitH = (>>= maybe empty (\ (a, q) -> pure a <|> altSplitH q)) . runSplitH

instance Applicative m => Applicative (SplitH m) where
  pure a = SplitH (pure (Just (a, SplitH (pure Nothing))))

  liftA2 f a b = SplitH (liftA2 (liftA2 (uncurry (***) . (f *** liftA2 f))) (runSplitH a) (runSplitH b))

instance Monad m => Alternative (SplitH m) where
  empty = SplitH (pure Nothing)

  a <|> b = SplitH (runSplitH a >>= maybe (runSplitH b) (\ (a', q) -> pure (Just (a', q <|> b))))

instance Monad m => Monad (SplitH m) where
  return = pure

  a >>= k = SplitH (runSplitH a >>= runSplitH . maybe empty (\ (a', q) -> k a' <|> (q >>= k)))

instance Carrier [] SplitH where
  joinl a = SplitH (a >>= runSplitH)

  suspend = SplitH (pure (Just ([()], empty)))

  resume []     = pure []
  resume (a:as) = runSplitH a >>= maybe (resume as) (\ (a', q) -> (a' :) <$> resume (q : as))

  wrap a = SplitH (a >>= \ a' -> case a' of
    []     -> pure Nothing
    a'':as -> pure (Just (a'', wrap (pure as))))
