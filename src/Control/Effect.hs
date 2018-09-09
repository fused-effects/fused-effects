{-# LANGUAGE EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, PolyKinds, RankNTypes, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Control.Effect where

import Control.Applicative (Alternative(..))
import Control.Monad ((<=<), ap, join, liftM)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Functor.Identity
import Prelude hiding (fail)

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) a)


class Effect sig where
  emap :: Monad m => (m a -> m b) -> (sig m a -> sig m b)

  handle :: (Monad m, Monad n, Functor c) => c () -> (forall x . c (m x) -> n (c x)) -> (sig m a -> sig n (c a))

inject :: Subset effect sig => effect (Eff sig) a -> Eff sig a
inject = Eff . inj

project :: Subset effect sig => Eff sig a -> Maybe (effect (Eff sig) a)
project (Eff op) = prj op
project _        = Nothing


data Void m a

instance Effect Void where
  emap _ v = case v of {}
  handle _ _ v = case v of {}

run :: Eff Void a -> a
run (Return a) = a
run (Eff v) = case v of {}


newtype Lift sig m a = Lift { unLift :: sig (m a) }

instance Functor sig => Effect (Lift sig) where
  emap f (Lift op) = Lift (fmap f op)
  handle state handler (Lift op) = Lift (fmap (\ p -> handler (p <$ state)) op)

instance Subset (Lift IO) sig => MonadIO (Eff sig) where
  liftIO = inject . Lift . fmap pure

runM :: Monad m => Eff (Lift m) a -> m a
runM (Return a) = return a
runM (Eff (Lift op)) = op >>= runM


data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)

instance (Effect l, Effect r) => Effect (l :+: r) where
  emap f (L l) = L (emap f l)
  emap f (R r) = R (emap f r)

  handle state handler (L l) = L (handle state handler l)
  handle state handler (R r) = R (handle state handler r)


pattern Other :: r (Eff (l :+: r)) a -> Eff (l :+: r) a
pattern Other s = Eff (R s)


data NonDet m a
  = Empty'
  | Choose' (Bool -> m a)

instance Effect NonDet where
  emap _ Empty' = Empty'
  emap f (Choose' k) = Choose' (f . k)

  handle _ _ Empty' = Empty'
  handle state handler (Choose' k) = Choose' (handler . (<$ state) . k)

pattern Empty :: Subset NonDet effects => Eff effects a
pattern Empty <- (project -> Just Empty')

pattern Choose :: Subset NonDet effects => (Bool -> Eff effects a) -> Eff effects a
pattern Choose k <- (project -> Just (Choose' k))

{-# COMPLETE Return, Empty, Choose, Other #-}

instance Subset NonDet sig => Alternative (Eff sig) where
  empty = inject Empty'
  l <|> r = inject (Choose' (\ c -> if c then l else r))

runNonDet :: Effect sig => Eff (NonDet :+: sig) a -> Eff sig [a]
runNonDet (Return a) = pure [a]
runNonDet Empty      = pure []
runNonDet (Choose k) = (++) <$> runNonDet (k True) <*> runNonDet (k False)
runNonDet (Other op) = Eff (handle [()] (fmap join . traverse runNonDet) op)


data Reader r m a
  = Ask' (r -> m a)
  | forall b . Local' (r -> r) (m b) (b -> m a)

instance Effect (Reader r) where
  emap f (Ask' k) = Ask' (f . k)
  emap f (Local' g m k) = Local' g m (f . k)

  handle state handler (Ask' k) = Ask' (handler . (<$ state) . k)
  handle state handler (Local' f m k) = Local' f (handler (m <$ state)) (handler . fmap k)

pattern Ask :: Subset (Reader r) effects => (r -> Eff effects a) -> Eff effects a
pattern Ask k <- (project -> Just (Ask' k))

pattern Local :: Subset (Reader r) effects => (r -> r) -> Eff effects b -> (b -> Eff effects a) -> Eff effects a
pattern Local f m k <- (project -> Just (Local' f m k))

{-# COMPLETE Return, Ask, Local, Other #-}

ask :: Subset (Reader r) sig => Eff sig r
ask = inject (Ask' pure)

local :: Subset (Reader r) sig => (r -> r) -> Eff sig a -> Eff sig a
local f m = inject (Local' f m pure)

runReader :: Effect sig => r -> Eff (Reader r :+: sig) a -> Eff sig a
runReader _ (Return a)    = pure a
runReader r (Ask k)       = runReader r (k r)
runReader r (Local f m k) = runReader (f r) m >>= runReader r . k
runReader r (Other op)    = runIdentity <$> Eff (handle (Identity ()) (fmap Identity . runReader r . runIdentity) op)


data State s m a
  = Get' (s -> m a)
  | Put' s (m a)

instance Effect (State s) where
  emap f (Get' k) = Get' (f . k)
  emap f (Put' s k) = Put' s (f k)

  handle state handler (Get' k) = Get' (handler . (<$ state) . k)
  handle state handler (Put' s k) = Put' s (handler (k <$ state))

pattern Get :: Subset (State s) effects => (s -> Eff effects a) -> Eff effects a
pattern Get k <- (project -> Just (Get' k))

pattern Put :: Subset (State s) effects => s -> Eff effects a -> Eff effects a
pattern Put s k <- (project -> Just (Put' s k))

{-# COMPLETE Return, Get, Put, Other #-}

get :: Subset (State s) sig => Eff sig s
get = inject (Get' pure)

put :: Subset (State s) sig => s -> Eff sig ()
put s = inject (Put' s (pure ()))

runState :: Effect sig => s -> Eff (State s :+: sig) a -> Eff sig (s, a)
runState s (Return a) = pure (s, a)
runState s (Get k)    = runState s (k s)
runState _ (Put s k)  = runState s k
runState s (Other op) = Eff (handle (s, ()) (uncurry runState) op)


data Fail m a = Fail' String

instance Effect Fail where
  emap _ (Fail' s) = Fail' s

  handle _ _ (Fail' s) = Fail' s

pattern Fail :: Subset Fail effects => String -> Eff effects a
pattern Fail s <- (project -> Just (Fail' s))

{-# COMPLETE Return, Fail, Other #-}

instance Subset Fail sig => MonadFail (Eff sig) where
  fail = inject . Fail'

runFail :: Effect sig => Eff (Fail :+: sig) a -> Eff sig (Either String a)
runFail (Return a) = pure (Right a)
runFail (Fail s)   = pure (Left s)
runFail (Other op) = Eff (handle (Right ()) (either (pure . Left) runFail) op)


data Exc exc m a
  = Throw' exc
  | forall b . Catch' (m b) (exc -> m b) (b -> m a)

instance Effect (Exc exc) where
  emap _ (Throw' exc) = Throw' exc
  emap f (Catch' m h k) = Catch' m h (f . k)

  handle _ _ (Throw' exc) = Throw' exc
  handle state handler (Catch' m h k) = Catch' (handler (m <$ state)) (handler . (<$ state) . h) (handler . fmap k)

pattern Throw :: Subset (Exc exc) effects => exc -> Eff effects a
pattern Throw exc <- (project -> Just (Throw' exc))

pattern Catch :: Subset (Exc exc) effects => Eff effects b -> (exc -> Eff effects b) -> (b -> Eff effects a) -> Eff effects a
pattern Catch m h k <- (project -> Just (Catch' m h k))

{-# COMPLETE Return, Throw, Catch, Other #-}

throw :: Subset (Exc exc) sig => exc -> Eff sig a
throw = inject . Throw'

catch :: Subset (Exc exc) sig => Eff sig a -> (exc -> Eff sig a) -> Eff sig a
catch m h = inject (Catch' m h pure)

runExc :: Effect sig => Eff (Exc exc :+: sig) a -> Eff sig (Either exc a)
runExc (Return a)    = pure (Right a)
runExc (Throw e)     = pure (Left e)
runExc (Catch m h k) = runExc m >>= either (either (pure . Left) (runExc . k) <=< runExc . h) (runExc . k)
runExc (Other op)    = Eff (handle (Right ()) (either (pure . Left) runExc) op)


data Resumable exc m a
  = forall b . Resumable' (exc b) (b -> m a)

instance Effect (Resumable exc) where
  emap f (Resumable' exc k) = Resumable' exc (f . k)

  handle state handler (Resumable' exc k) = Resumable' exc (handler . (<$ state) . k)

pattern Resumable :: Subset (Resumable exc) effects => exc b -> (b -> Eff effects a) -> Eff effects a
pattern Resumable exc k <- (project -> Just (Resumable' exc k))

{-# COMPLETE Return, Resumable, Other #-}

throwResumable :: Subset (Resumable exc) sig => exc a -> Eff sig a
throwResumable exc = inject (Resumable' exc pure)

runResumable :: Effect sig => (forall resume . exc resume -> Eff sig resume) -> Eff (Resumable exc :+: sig) a -> Eff sig a
runResumable _ (Return a)        = pure a
runResumable f (Resumable exc k) = f exc >>= runResumable f . k
runResumable f (Other op)        = runIdentity <$> Eff (handle (Identity ()) (fmap Identity . runResumable f . runIdentity) op)


data Cut m a = Cut'

instance Effect Cut where
  emap _ Cut' = Cut'
  handle _ _ Cut' = Cut'

pattern Cut :: Subset Cut sig => Eff sig a
pattern Cut <- (project -> Just Cut')


data Symbol m a
  = Symbol' (Char -> Bool) (Char -> m a)

instance Effect Symbol where
  emap f (Symbol' sat k) = Symbol' sat (f . k)

  handle state handler (Symbol' sat k) = Symbol' sat (handler . (<$ state) . k)

pattern Symbol :: Subset Symbol sig => (Char -> Bool) -> (Char -> Eff sig a) -> Eff sig a
pattern Symbol sat k <- (project -> Just (Symbol' sat k))

{-# COMPLETE Return, Symbol, Other #-}

satisfy :: Subset Symbol sig => (Char -> Bool) -> Eff sig Char
satisfy sat = inject (Symbol' sat pure)

char :: Subset Symbol sig => Char -> Eff sig Char
char c = satisfy (== c)

digit :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Char
digit = foldr ((<|>) . char) empty ['0'..'9']

expr :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Int
expr = do
  i <- term
  (i +) <$ char '+' <*> expr <|> pure i

term :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Int
term = do
  i <- factor
  (i *) <$ char '*' <*> term <|> pure i

factor :: (Subset NonDet sig, Subset Symbol sig) => Eff sig Int
factor = read <$> some digit
     <|> char '(' *> expr <* char ')'

runSymbol :: Subset NonDet sig => String -> Eff (Symbol :+: sig) a -> Eff sig a
runSymbol ""     (Return a)               = pure a
runSymbol _      (Return _)               = empty
runSymbol ""     (Symbol _ _)             = empty
runSymbol (c:cs) (Symbol p k) | p c       = runSymbol cs (k c)
                              | otherwise = empty
runSymbol cs     (Other op)               = runIdentity <$> Eff (handle (Identity ()) (fmap Identity . runSymbol cs . runIdentity) op)


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
  Return v >>= k = k v
  Eff op >>= k = Eff (emap (>>= k) op)
