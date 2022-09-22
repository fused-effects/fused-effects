{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use fmap" #-}
module MonadFix
( test
) where

import Control.Monad (liftM)
import Control.Monad.Fix
import Gen

test
  :: (MonadFix m, Arg a, Eq (g a), Eq (g b), Functor f, Show a, Show (g a), Show (g b), Vary a)
  => GenM m
  -> GenTerm a
  -> GenTerm b
  -> GenTerm (f ())
  -> Run f g m
  -> [TestTree]
test m a b s (Run run) =
  [ testProperty "purity" . forall_ (s :. termFn a :. Nil) $
    \ s h -> run (mfix (return . h) <$ s) === run (return (fix h) <$ s)
  , testProperty "left-shrinking" . forall_ (s :. m a :. termFn (fn (m b)) :. Nil) $
    \ s m f -> run (mfix (\ x -> m >>= \ y -> f x y) <$ s) === run ((m >>= \ y -> mfix (\ x -> f x y)) <$ s)
  , testProperty "sliding" . forall_ (s :. fn b :. termFn (m a) :. Nil) $
    \ s h f -> run (mfix (liftM h . f) <$ s) === run (liftM h (mfix (f . h)) <$ s)
  , testProperty "nesting" . forall_ (s :. termFn (termFn (m a)) :. Nil) $
    \ s f -> run (mfix (\ x -> mfix (\ y -> f x y)) <$ s) === run (mfix (\ x -> f x x) <$ s)
  ]
