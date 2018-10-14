{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeApplications, UndecidableInstances #-}
module Control.Effect.IO
( catchIO
, bracket
) where

import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Lift
import Control.Effect.Sum
import qualified Control.Exception as Exc
import Control.Monad.IO.Class

-- | Generalize 'Exc.catch' to other 'MonadIO' contexts for the handler and result.
catchIO :: ( Exc.Exception exc
           , MonadIO m
           )
        => IO a
        -> (exc -> m a)
        -> m a
catchIO m handler = liftIO (Exc.try m) >>= either handler pure


-- | The semantics of @bracket before after handler@ are as follows:
--
--   * Exceptions in @before@ and @after@ are thrown in IO.
--   * @after@ is called on IO exceptions in @handler@, and then rethrown in IO.
--   * If @handler@ completes successfully, @after@ is called.
--
--   Call 'catchIO' at the call site if you want to recover.
bracket :: ( Member (Lift IO) sig
           , Carrier sig m
           , MonadIO m
           )
        => IO a
        -> (a -> IO b)
        -> (a -> Eff (BracketC m) c)
        -> m c
bracket before after action = do
  a <- liftIO before
  let cleanup = after a
  res <- runBracketC cleanup (interpret (action a))
  res <$ liftIO cleanup

newtype BracketC m a = BracketC (forall b . IO b -> m a)

runBracketC :: IO b -> BracketC m a -> m a
runBracketC io (BracketC m) = m io

instance (Member (Lift IO) sig, Carrier sig m, MonadIO m) => Carrier sig (BracketC m) where
  gen a = BracketC (const (gen a))
  alg op
    | Just (Lift m) <- prj op = BracketC (\ cleanup -> liftIO (Exc.try m) >>= either (\ exc -> liftIO cleanup >> liftIO (Exc.throwIO @Exc.SomeException exc)) (runBracketC cleanup))
    | otherwise               = BracketC (\ cleanup -> alg (handlePure (runBracketC cleanup) op))
