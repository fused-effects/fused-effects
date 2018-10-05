{-# LANGUAGE DefaultSignatures, DeriveFunctor, EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, PolyKinds, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Control.Effect
( Eff
, send
, fold
, liftAlg
, relay
, Effect(..)
, Carrier(..)
, Void
, run
, Lift(..)
, runM
, NonDet(..)
, runNonDet
, Reader(..)
, ask
, local
, runReader
, State(..)
, get
, put
, runState
, Fail(..)
, runFail
, Exc(..)
, throw
, catch
, runExc
, Resumable(..)
, throwResumable
, runResumable
, Cut(..)
, cutfail
, call
, cut
, Symbol(..)
, satisfy
, char
, digit
, expr
, term
, factor
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad ((<=<), ap, liftM)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Functor.Identity
import Prelude hiding (fail)

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) (Eff effects a))

class Effect sig where
  fmap' :: (a -> b) -> (sig m a -> sig m b)
  default fmap' :: Functor (sig m) => (a -> b) -> (sig m a -> sig m b)
  fmap' = fmap

  handle :: (Carrier c f, Monad m, Monad n, Applicative (c n))
         => f ()
         -> (forall x . f (m x) -> n (f x))
         -> sig m (c n a)
         -> sig n (c n a)


send :: Subset effect sig => effect (Eff sig) (Eff sig a) -> Eff sig a
send = Eff . inj


fold :: Effect sig
     => (a -> b)
     -> (sig (Eff sig) b -> b)
     -> (Eff sig a -> b)
fold gen alg = go
  where go (Return x) = gen x
        go (Eff op)   = alg (fmap' go op)

liftAlg :: (Effect eff, Effect sig, Carrier c f, Monad (c (Eff sig)))
        => (forall a .  eff          (Eff (eff :+: sig)) (c (Eff sig) a) -> c (Eff sig) a)
        -> (forall a . (eff :+: sig) (Eff (eff :+: sig)) (c (Eff sig) a) -> c (Eff sig) a)
liftAlg alg1 = alg1 \/ alg2
  where alg2 op = suspend >>= \ state -> joinl (Eff (fmap' pure (handle state (resume . fmap (fold pure (liftAlg alg1))) op)))

relay :: (Effect eff, Effect sig, Carrier c f, Monad (c (Eff sig)))
      => (forall a . eff (Eff (eff :+: sig)) (c (Eff sig) a) -> c (Eff sig) a)
      -> (Eff (eff :+: sig) a -> c (Eff sig) a)
relay alg = fold pure (liftAlg alg)
{-# INLINE relay #-}


class Functor f => Carrier c f | c -> f where
  -- | (Left-)join a 'Monad' of 'Carrier's into a 'Carrier'.
  -- @
  -- joinl . pure = id
  -- @
  --
  -- @
  -- joinl . join = joinl . fmap joinl
  -- @
  joinl :: Monad m => m (c m a) -> c m a

  suspend :: Monad m => c m (f ())

  resume :: Monad m => f (c m a) -> m (f a)

  wrap :: Monad m => m (f a) -> c m a


newtype IdH m a = IdH { runIdH :: m a }
  deriving (Applicative, Functor, Monad)

instance Carrier IdH Identity where
  joinl mf = IdH (mf >>= runIdH)

  suspend = IdH (pure (Identity ()))

  resume = fmap Identity . runIdH . runIdentity

  wrap = IdH . fmap runIdentity


newtype StateH s m a = StateH { runStateH :: s -> m (s, a) }
  deriving (Functor)

instance Monad m => Applicative (StateH s m) where
  pure a = StateH (\ s -> pure (s, a))

  StateH f <*> StateH a = StateH $ \ s -> do
    (s',  f') <- f s
    (s'', a') <- a s'
    let fa = f' a'
    fa `seq` pure (s'', fa)

instance Monad m => Monad (StateH s m) where
  return = pure

  StateH a >>= f = StateH $ \ s -> do
    (s', a') <- a s
    let fa = f a'
    fa `seq` runStateH fa s'

instance Carrier (StateH s) ((,) s) where
  joinl mf = StateH (\ s -> mf >>= \ f -> runStateH f s)

  suspend = StateH (\ s -> pure (s, (s, ())))

  resume (s, m) = runStateH m s

  wrap = StateH . const


newtype ReaderH r m a = ReaderH { runReaderH :: r -> m a }
  deriving (Functor)

instance Applicative m => Applicative (ReaderH r m) where
  pure a = ReaderH (\ _ -> pure a)

  ReaderH f <*> ReaderH a = ReaderH (\ r -> f r <*> a r)

instance Monad m => Monad (ReaderH r m) where
  return = pure

  ReaderH a >>= f = ReaderH (\ r -> a r >>= \ a' -> runReaderH (f a') r)

instance Carrier (ReaderH r) ((,) r) where
  joinl mf = ReaderH (\ r -> mf >>= \ f -> runReaderH f r)

  suspend = ReaderH (\ r -> pure (r, ()))

  resume (r, m) = (,) r <$> runReaderH m r

  wrap = ReaderH . const . fmap snd


newtype WriterH w m a = WriterH { runWriterH :: m (w, a) }
  deriving (Functor)

instance (Monoid w, Applicative m) => Applicative (WriterH w m) where
  pure a = WriterH (pure (mempty, a))

  WriterH f <*> WriterH a = WriterH (liftA2 (<*>) f a)

instance (Monoid w, Monad m) => Monad (WriterH w m) where
  return = pure

  WriterH a >>= f = WriterH (do
    (w1, a') <- a
    let fa = f a'
    (w2, a'') <- fa `seq` runWriterH fa
    let w = w1 <> w2
    w `seq` pure (w, a''))

instance Monoid w => Carrier (WriterH w) ((,) w) where
  joinl mf = WriterH (mf >>= runWriterH)

  suspend = WriterH (pure (mempty, (mempty, ())))

  resume (w, m) = first (w <>) <$> runWriterH m

  wrap = WriterH


newtype MaybeH m a = MaybeH { runMaybeH :: m (Maybe a) }
  deriving (Functor)

instance Applicative m => Applicative (MaybeH m) where
  pure a = MaybeH (pure (Just a))

  MaybeH f <*> MaybeH a = MaybeH (liftA2 (<*>) f a)

instance Monad m => Monad (MaybeH m) where
  return = pure

  MaybeH a >>= f = MaybeH (a >>= maybe (pure Nothing) (runMaybeH . f))

instance Carrier MaybeH Maybe where
  joinl mf = MaybeH (mf >>= runMaybeH)

  suspend = MaybeH (pure (Just (Just ())))

  resume = maybe (pure Nothing) runMaybeH

  wrap = MaybeH


newtype EitherH e m a = EitherH { runEitherH :: m (Either e a) }
  deriving (Functor)

instance Applicative m => Applicative (EitherH e m) where
  pure a = EitherH (pure (Right a))

  EitherH f <*> EitherH a = EitherH (liftA2 (<*>) f a)

instance Monad m => Monad (EitherH e m) where
  return = pure

  EitherH a >>= f = EitherH (a >>= either (pure . Left) (runEitherH . f))

instance Carrier (EitherH e) (Either e) where
  joinl mf = EitherH (mf >>= runEitherH)

  suspend = EitherH (pure (Right (Right ())))

  resume = either (pure . Left) runEitherH

  wrap = EitherH


newtype ListH m a = ListH { runListH :: m [a] }
  deriving (Functor)

instance Applicative m => Applicative (ListH m) where
  pure a = ListH (pure [a])

  ListH f <*> ListH a = ListH (liftA2 (<*>) f a)

instance Monad m => Monad (ListH m) where
  return = pure

  ListH a >>= f = ListH (a >>= fmap concat . traverse (runListH . f))

instance Carrier ListH [] where
  joinl mf = ListH (mf >>= runListH)

  suspend = ListH (pure [[()]])

  resume = fmap concat . traverse runListH

  wrap = ListH


data Void m k
  deriving (Functor)

instance Effect Void where
  handle _ _ v = case v of {}

run :: Eff Void a -> a
run = fold id (\ v -> case v of {})


newtype Lift sig m k = Lift { unLift :: sig k }
  deriving (Functor)

instance Functor sig => Effect (Lift sig) where
  handle _ _ (Lift op) = Lift op

instance Subset (Lift IO) sig => MonadIO (Eff sig) where
  liftIO = send . Lift . fmap pure

runM :: Monad m => Eff (Lift m) a -> m a
runM (Return a)      = pure a
runM (Eff (Lift op)) = op >>= runM


data (f :+: g) (m :: * -> *) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Functor, Ord, Show)

instance (Effect l, Effect r) => Effect (l :+: r) where
  fmap' f (L l) = L (fmap' f l)
  fmap' f (R r) = R (fmap' f r)

  handle state handler (L l) = L (handle state handler l)
  handle state handler (R r) = R (handle state handler r)

(\/) :: ( sig1           m a -> b)
     -> (          sig2  m a -> b)
     -> ((sig1 :+: sig2) m a -> b)
(alg1 \/ _   ) (L op) = alg1 op
(_    \/ alg2) (R op) = alg2 op


data NonDet m k
  = Empty
  | Choose (Bool -> k)
  deriving (Functor)

instance Effect NonDet where
  handle _ _ Empty      = Empty
  handle _ _ (Choose k) = Choose k

instance Subset NonDet sig => Alternative (Eff sig) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

runNonDet :: Effect sig => Eff (NonDet :+: sig) a -> Eff sig [a]
runNonDet = runListH . relay alg
  where alg Empty      = ListH (pure [])
        alg (Choose k) = ListH (liftA2 (++) (runListH (k True)) (runListH (k False)))


data Reader r m k
  = Ask (r -> k)
  | forall b . Local (r -> r) (m b) (b -> k)

deriving instance Functor (Reader r m)

instance Effect (Reader r) where
  handle _     _       (Ask k)       = Ask k
  handle state handler (Local f m k) = Local f (handler (m <$ state)) (wrap . resume . fmap k)

ask :: Subset (Reader r) sig => Eff sig r
ask = send (Ask pure)

local :: Subset (Reader r) sig => (r -> r) -> Eff sig a -> Eff sig a
local f m = send (Local f m pure)


runReader :: Effect sig => r -> Eff (Reader r :+: sig) a -> Eff sig a
runReader r m = runReaderH (relay alg m) r
  where alg (Ask k)       = ReaderH (\ r -> runReaderH (k r) r)
        alg (Local f m k) = ReaderH (\ r -> let fr = f r in fr `seq` runReader fr m >>= flip runReaderH r . k)


data State s m k
  = Get (s -> k)
  | Put s k
  deriving (Functor)

instance Effect (State s) where
  handle _ _ (Get k)   = Get   k
  handle _ _ (Put s k) = Put s k

get :: Subset (State s) sig => Eff sig s
get = send (Get pure)

put :: Subset (State s) sig => s -> Eff sig ()
put s = send (Put s (pure ()))

runState :: Effect sig => s -> Eff (State s :+: sig) a -> Eff sig (s, a)
runState s m = runStateH (relay alg m) s
  where alg (Get k)   = StateH (\ s -> runStateH (k s) s)
        alg (Put s k) = StateH (\ _ -> runStateH  k    s)


newtype Fail m k = Fail String
  deriving (Functor)

instance Effect Fail where
  handle _ _ (Fail s) = Fail s

instance Subset Fail sig => MonadFail (Eff sig) where
  fail = send . Fail

runFail :: Effect sig => Eff (Fail :+: sig) a -> Eff sig (Either String a)
runFail = runEitherH . relay alg
  where alg (Fail s) = EitherH (pure (Left s))


data Exc exc m k
  = Throw exc
  | forall b . Catch (m b) (exc -> m b) (b -> k)

deriving instance Functor (Exc exc m)

instance Effect (Exc exc) where
  handle _     _       (Throw exc)   = Throw exc
  handle state handler (Catch m h k) = Catch (handler (m <$ state)) (handler . (<$ state) . h) (wrap . resume . fmap k)

throw :: Subset (Exc exc) sig => exc -> Eff sig a
throw = send . Throw

catch :: Subset (Exc exc) sig => Eff sig a -> (exc -> Eff sig a) -> Eff sig a
catch m h = send (Catch m h pure)

runExc :: Effect sig => Eff (Exc exc :+: sig) a -> Eff sig (Either exc a)
runExc = runEitherH . relay alg
  where alg (Throw e)     = EitherH (pure (Left e))
        alg (Catch m h k) = EitherH (runExc m >>= either (runEitherH . (k <=< EitherH . runExc . h)) (runEitherH . k))


data Resumable exc m k
  = forall b . Resumable (exc b) (b -> k)

deriving instance Functor (Resumable exc m)

instance Effect (Resumable exc) where
  handle _ _ (Resumable exc k) = Resumable exc k

throwResumable :: Subset (Resumable exc) sig => exc a -> Eff sig a
throwResumable exc = send (Resumable exc pure)

runResumable :: Effect sig => (forall resume . exc resume -> Eff sig resume) -> Eff (Resumable exc :+: sig) a -> Eff sig a
runResumable f = runIdH . relay alg
  where alg (Resumable exc k) = IdH (f exc >>= runIdH . k)


data Cut m k
  = Cut
  | forall b . Call (m b) (b -> k)

deriving instance Functor (Cut m)

instance Effect Cut where
  handle _     _       Cut        = Cut
  handle state handler (Call m k) = Call (handler (m <$ state)) (wrap . resume . fmap k)

cutfail :: Subset Cut sig => Eff sig a
cutfail = send Cut

call :: Subset Cut sig => Eff sig a -> Eff sig a
call m = send (Call m pure)

cut :: (Subset NonDet sig, Subset Cut sig) => Eff sig ()
cut = skip <|> cutfail

skip :: Applicative m => m ()
skip = pure ()

-- runCut :: Subset NonDet sig => Eff (Cut :+: sig) a -> Eff sig a
-- runCut = go empty
--   where go :: Subset NonDet sig => Eff sig a -> Eff (Cut :+: sig) a -> Eff sig a
--         go q (Return a) = pure a <|> q
--         go q Empty      = q
--         go _ Cut        = empty
--         go q (Choose k) = go (go q (k False)) (k True)
--         go q (Call m k) = go empty m >>= go q . k
--         go q (Other op) = Eff (hfmap (go empty) op) <|> q


data Symbol m k
  = Symbol (Char -> Bool) (Char -> k)
  deriving (Functor)

instance Effect Symbol where
  handle _ _ (Symbol sat k) = Symbol sat k

satisfy :: Subset Symbol sig => (Char -> Bool) -> Eff sig Char
satisfy sat = send (Symbol sat pure)

char :: Subset Symbol sig => Char -> Eff sig Char
char c = satisfy (== c)

digit :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Char
digit = foldr ((<|>) . char) empty ['0'..'9']

expr :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr) <|> pure i

term :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term) <|> pure i

factor :: (Subset Cut sig, Subset NonDet sig, Subset Symbol sig) => Eff sig Int
factor = read <$> some digit
     <|> char '(' *> expr <* char ')'

-- parse :: Subset NonDet sig => String -> Eff (Symbol :+: sig) a -> Eff sig a
-- parse ""     (Return a)               = pure a
-- parse _      (Return _)               = empty
-- parse ""     (Symbol _ _)             = empty
-- parse (c:cs) (Symbol p k) | p c       = parse cs (k c)
--                           | otherwise = empty
-- parse cs     (Other op)               = runIdentity <$> Eff (handle (Identity ()) (fmap Identity . parse cs . runIdentity) op)


class (Effect sub, Effect sup) => Subset sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Effect sub => Subset sub sub where
  inj = id
  prj = Just

instance {-# OVERLAPPABLE #-} (Effect sub, Effect sup) => Subset sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance {-# OVERLAPPABLE #-} (Effect sub', Subset sub sup) => Subset sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


instance Effect sig => Functor (Eff sig) where
  fmap = liftM

instance Effect sig => Applicative (Eff sig) where
  pure = Return
  (<*>) = ap

instance Effect sig => Monad (Eff sig) where
  m >>= k = fold k Eff m

--
test1 :: String
test1
  = run
  ( runReader "world"
  ( snd <$> runState 'a'
  ( local ("hello, " ++) ((:) <$> get <*> ask))))

test2 :: String
test2
  = run
  ( runReader "world"
  ( local ("hello, " ++) ((:) <$> pure 'a' <*> ask)))
