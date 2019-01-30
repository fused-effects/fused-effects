{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Resource
( Resource(..)
, bracket
, bracketOnError
, runResource
, ResourceC(..)
) where

import           Control.Effect.Carrier
import           Control.Effect.Internal
import           Control.Effect.Sum
import qualified Control.Exception as Exc
import           Control.Monad.IO.Class

data Resource m k
  = forall resource any output . Resource (m resource) (resource -> m any) (resource -> m output) (output -> k)
  | forall resource any output . OnError  (m resource) (resource -> m any) (resource -> m output) (output -> k)

deriving instance Functor (Resource m)

instance HFunctor Resource where
  hmap f (Resource acquire release use k) = Resource (f acquire) (f . release) (f . use) k
  hmap f (OnError acquire release use k)  = OnError  (f acquire) (f . release) (f . use) k

instance Effect Resource where
  handle state handler (Resource acquire release use k) = Resource (handler (acquire <$ state)) (handler . fmap release) (handler . fmap use) (handler . fmap k)
  handle state handler (OnError acquire release use k)  = OnError  (handler (acquire <$ state)) (handler . fmap release) (handler . fmap use) (handler . fmap k)

-- | Provides a safe idiom to acquire and release resources safely.
--
-- When acquiring and operating on a resource (such as opening and
-- reading file handle with 'openFile' or writing to a blob of memory
-- with 'malloc'), any exception thrown during the operation may mean
-- that the resource is not properly released. @bracket acquire release op@
-- ensures that @release@ is run on the value returned from @acquire@ even
-- if @op@ throws an exception.
--
-- 'bracket' is safe in the presence of asynchronous exceptions.
bracket :: (Member Resource sig, Carrier sig m)
        => m resource           -- ^ computation to run first ("acquire resource")
        -> (resource -> m any)  -- ^ computation to run last ("release resource")
        -> (resource -> m a)    -- ^ computation to run in-between
        -> m a
bracket acquire release use = send (Resource acquire release use ret)

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError :: (Member Resource sig, Carrier sig m)
               => m resource           -- ^ computation to run first ("acquire resource")
               -> (resource -> m any)  -- ^ computation to run last ("release resource")
               -> (resource -> m a)    -- ^ computation to run in-between
               -> m a
bracketOnError acquire release use = send (OnError acquire release use ret)

runResource :: (Carrier sig m, MonadIO m)
            => (forall x . m x -> IO x)
            -> Eff (ResourceC m) a
            -> m a
runResource handler = runResourceC handler . interpret

newtype ResourceC m a = ResourceC ((forall x . m x -> IO x) -> m a)

runResourceC :: (forall x . m x -> IO x) -> ResourceC m a -> m a
runResourceC handler (ResourceC m) = m handler

instance (Carrier sig m, MonadIO m) => Carrier (Resource :+: sig) (ResourceC m) where
  ret a = ResourceC (const (ret a))
  eff op = ResourceC (\ handler -> handleSum
    (eff . handlePure (runResourceC handler))
    (\case
        Resource acquire release use k -> liftIO (Exc.bracket
                                                    (handler (runResourceC handler acquire))
                                                    (handler . runResourceC handler . release)
                                                    (handler . runResourceC handler . use))
                                            >>= runResourceC handler . k
        OnError acquire release use k -> liftIO (Exc.bracketOnError 
                                                    (handler (runResourceC handler acquire))
                                                    (handler . runResourceC handler . release)
                                                    (handler . runResourceC handler . use))
                                            >>= runResourceC handler . k
    ) op)
