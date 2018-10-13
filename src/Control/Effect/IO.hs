{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TypeApplications, UndecidableInstances #-}
module Control.Effect.IO
( rethrowing
, bracket
) where

import Control.Effect.Error
import Control.Effect.Handler
import Control.Effect.Internal
import Control.Effect.Lift
import Control.Effect.Sum
import qualified Control.Exception as Exc
import Control.Monad.IO.Class

-- | Lift an 'IO' action into a carrier, catching and rethrowing any exceptions it throws into an 'Error' effect.
--
--   If you need more granular control over the types of exceptions caught, use 'catchIO' and rethrow in the handler.
rethrowing :: ( Member (Error Exc.SomeException) sig
              , MonadIO m
              , Carrier sig m
              )
           => IO a
           -> m a
rethrowing m = liftIO (Exc.try m) >>= either (throwError . Exc.toException @Exc.SomeException) pure


-- | The semantics of @bracket before after handler@ are as follows:
--
--   * Exceptions in @before@ and @after@ are thrown in IO.
--   * @after@ is called on IO exceptions in @handler@, and then rethrown in IO.
--   * If @handler@ completes successfully, @after@ is called
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
