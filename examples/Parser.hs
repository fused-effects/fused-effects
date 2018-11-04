{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, KindSignatures, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Parser where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Cut
import Control.Effect.NonDet
import Control.Effect.Sum hiding (L)
import Control.Monad (replicateM)
import Data.Char
import Data.Coerce
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Monoid (Alt(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "parser" $ do
  describe "parse" $ do
    prop "returns pure values at the end of input" $
      \ a -> run (runNonDet (parse "" (pure a))) == [a :: Integer]

    prop "fails if input remains" $
      \ c cs a -> run (runNonDet (parse (c:cs) (pure (a :: Integer)))) == []

  describe "satisfy" $ do
    prop "matches with a predicate" $
      \ c f -> run (runNonDet (parse [c] (satisfy (applyFun f)))) == if applyFun f c then [c] else []

    prop "fails at end of input" $
      \ f -> run (runNonDet (parse "" (satisfy (applyFun f)))) == []

    prop "fails if input remains" $
      \ c1 c2 f -> run (runNonDet (parse [c1, c2] (satisfy (applyFun f)))) == []

    prop "consumes input" $
      \ c1 c2 f -> run (runNonDet (parse [c1, c2] ((,) <$> satisfy (applyFun f) <*> satisfy (applyFun f)))) == if applyFun f c1 && applyFun f c2 then [(c1, c2)] else []

  describe "factor" $ do
    prop "matches positive integers" $
      \ a -> run (runNonDetOnce (runCut2 (parse (show (abs a)) factor))) == [abs a]

    prop "matches parenthesized expressions" . forAll (sized arbNested) $
      \ as -> run (runNonDetOnce (runCut2 (parse ('(' : intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as) ++ ")") factor))) == [sum (map (product . map abs) as)]

  describe "term" $ do
    prop "matches factors" $
      \ a -> run (runNonDetOnce (runCut2 (parse (show (abs a)) term))) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDetOnce (runCut2 (parse (intercalate "*" (show . abs <$> 1:as)) term))) == [product (map abs as)]

  describe "expr" $ do
    prop "matches factors" $
      \ a -> run (runNonDetOnce (runCut2 (parse (show (abs a)) expr))) == [abs a]

    prop "matches multiplication" $
      \ as -> run (runNonDetOnce (runCut2 (parse (intercalate "*" (show . abs <$> 1:as)) expr))) == [product (map abs as)]

    prop "matches addition" $
      \ as -> run (runNonDetOnce (runCut2 (parse (intercalate "+" (show . abs <$> 0:as)) expr))) == [sum (map abs as)]

    prop "respects order of operations" . forAll (sized arbNested) $
      \ as -> run (runNonDetOnce (runCut2 (parse (intercalate "+" (intercalate "*" . map (show . abs) . (1:) <$> [0]:as)) expr))) == [sum (map (product . map abs) as)]

    where arbNested :: Arbitrary a => Int -> Gen [[a]]
          arbNested 0 = pure []
          arbNested n = do
            Positive m <- arbitrary
            let n' = n `div` (m + 1)
            replicateM m (vector n')


data Symbol (m :: * -> *) k = Satisfy (Char -> Bool) (Char -> k)
  deriving (Functor)

instance HFunctor Symbol where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Symbol where
  handle state handler = coerce . fmap (handler . (<$ state))

satisfy :: (Carrier sig m, Member Symbol sig) => (Char -> Bool) -> m Char
satisfy p = send (Satisfy p ret)

char :: (Carrier sig m, Member Symbol sig) => Char -> m Char
char = satisfy . (==)

digit :: (Carrier sig m, Member Symbol sig) => m Char
digit = satisfy isDigit

parens :: (Applicative m, Carrier sig m, Member Symbol sig) => m a -> m a
parens m = char '(' *> m <* char ')'


parse :: (Alternative m, Carrier sig m, Effect sig, Monad m) => String -> Eff (ParseC m) a -> m a
parse input = (>>= exhaustive) . flip runParseC input . interpret
  where exhaustive ("", a) = pure a
        exhaustive _       = empty

newtype ParseC m a = ParseC { runParseC :: String -> m (String, a) }

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Symbol :+: sig) (ParseC m) where
  ret a = ParseC (\ input -> ret (input, a))
  {-# INLINE ret #-}

  eff op = ParseC (\ input -> handleSum
    (eff . handleState input runParseC)
    (\ (Satisfy p k) -> case input of
      c:cs | p c -> runParseC (k c) cs
      _          -> empty)
    op)
  {-# INLINE eff #-}


expr :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig, Monad m) => m Int
expr = do
  i <- term
  call ((i +) <$ char '+' <* cut <*> expr
    <|> pure i)

term :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig, Monad m) => m Int
term = do
  i <- factor
  call ((i *) <$ char '*' <* cut <*> term
    <|> pure i)

factor :: (Alternative m, Carrier sig m, Member Cut sig, Member Symbol sig, Monad m) => m Int
factor
  =   read <$> some digit
  <|> parens expr


data B a = E | L a | B (B a) (B a)
  deriving (Functor)

bList :: B a -> [a]
bList = toList

instance Foldable B where
  toList = flip go []
    where go E       rest = rest
          go (L a)   rest = a : rest
          go (B a b) rest = go a (go b rest)

  foldMap f = go
    where go E       = mempty
          go (L a)   = f a
          go (B a b) = go a <> go b

  null E = True
  null _ = False

instance Traversable B where
  traverse f = go
    where go E       = pure E
          go (L a)   = L <$> f a
          go (B a b) = B <$> go a <*> go b

instance Applicative B where
  pure = L
  E     <*> _ = E
  L f   <*> a = fmap f a
  B l r <*> a = B (l <*> a) (r <*> a)

instance Alternative B where
  empty = E
  E <|> b = b
  a <|> E = a
  a <|> b = B a b

instance Monad B where
  return = pure
  E     >>= _ = E
  L a   >>= f = f a
  B l r >>= f = B (l >>= f) (r >>= f)


data Branch1 f a
  = Prune1
  | None1
  | Some1 (f a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

branch1 :: a -> a -> (f b -> a) -> Branch1 f b -> a
branch1 a _ _ Prune1    = a
branch1 _ a _ None1     = a
branch1 _ _ f (Some1 a) = f a

joinBranch1 :: (Alternative f, Monad f) => f (Branch1 f a) -> Branch1 f a
joinBranch1 = Some1 . (>>= branch1 empty empty id)

instance Applicative f => Applicative (Branch1 f) where
  pure = Some1 . pure

  Prune1  <*> _       = Prune1
  _       <*> Prune1  = Prune1
  None1   <*> _       = None1
  _       <*> None1   = None1
  Some1 f <*> Some1 a = Some1 (f <*> a)

instance (Alternative f, Monad f) => Monad (Branch1 f) where
  return = pure

  Prune1  >>= _ = Prune1
  None1   >>= _ = None1
  Some1 a >>= f = Some1 (a >>= branch1 empty empty id . f)


runCutAll :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (CutAllC B m) a -> m a
runCutAll = (>>= branch1 empty empty (getAlt . foldMap (Alt . pure))) . runCutAllC . interpret

newtype CutAllC f m a = CutAllC { runCutAllC :: m (Branch1 f a) }

instance (Alternative f, Carrier sig m, Effect sig, Monad f, Monad m, Traversable f) => Carrier (Cut :+: NonDet :+: sig) (CutAllC f m) where
  ret = CutAllC . ret . Some1 . pure
  eff = CutAllC . handleSum (handleSum
    (eff . handleTraversable runCutAllC)
    (\case
      Empty    -> ret None1
      Choose k -> runCutAllC (k True) >>= branch1 (ret Prune1) (runCutAllC (k False)) (\ a -> runCutAllC (k False) >>= branch1 (ret (Some1 a)) (ret (Some1 a)) (ret . Some1 . (a <|>)))))
    (\case
      Cutfail  -> ret Prune1
      Call m k -> runCutAllC m >>= branch1 (ret None1) (ret None1) (fmap joinBranch1 . traverse (runCutAllC . k)))


data Branch2 m a
  = Prune2
  | None2
  | Some2 a
  | More2 (m (Branch2 m a)) (m (Branch2 m a))
  deriving (Functor)

branch2 :: a -> a -> (b -> a) -> (m (Branch2 m b) -> m (Branch2 m b) -> a) -> Branch2 m b -> a
branch2 a _ _ _ Prune2      = a
branch2 _ a _ _ None2       = a
branch2 _ _ f _ (Some2 a)   = f a
branch2 _ _ _ f (More2 a b) = f a b

joinBranch2 :: (Alternative m, Monad m) => Branch2 m a -> m a
joinBranch2 = branch2 empty empty pure alt
  where alt a b = (a >>= joinBranch2) <|> (b >>= joinBranch2)

runCut2 :: (Alternative m, Carrier sig m, Effect sig, Monad m) => Eff (Cut2C m) a -> m a
runCut2 = (>>= joinBranch2) . runCut2C . interpret

newtype Cut2C m a = Cut2C { runCut2C :: m (Branch2 m a) }

instance (Alternative m, Carrier sig m, Effect sig, Monad m) => Carrier (Cut :+: NonDet :+: sig) (Cut2C m) where
  ret = Cut2C . ret . Some2
  eff = Cut2C . handleSum (handleSum
    (eff . handle (Some2 ()) (branch2
      (ret Prune2)
      (ret None2)
      runCut2C
      (\ a b -> ret (More2 (a >>= joinBranch2 >>= runCut2C) (b >>= joinBranch2 >>= runCut2C)))))
    (\case
      Empty    -> ret None2
      Choose k -> runCut2C (k True) >>= branch2
        (ret Prune2)
        (runCut2C (k False))
        (\ a -> ret (More2 (ret (Some2 a)) (runCut2C (k False))))
        (fmap ret . More2)))
    (\case
      Cutfail  -> ret Prune2
      Call m k -> runCut2C m >>= branch2
        (ret None2)
        (ret None2)
        (runCut2C . k)
        (\ a b -> ret (More2 (a >>= joinBranch2 >>= runCut2C . k) (b >>= joinBranch2 >>= runCut2C . k))))
-- (\ a b -> (a >>= joinBranch2 >>= runCut2C) <|> (b >>= joinBranch2 >>= runCut2C))
