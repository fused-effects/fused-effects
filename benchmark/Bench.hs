{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, RankNTypes, TypeApplications, TypeOperators, UndecidableInstances #-}
module Main
( main
) where

import Control.Algebra
import Control.Carrier.Interpret
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Monad (ap, replicateM_)
import Data.Functor.Identity
import Data.Monoid (Sum(..))
import Gauge

import qualified NonDet

main :: IO ()
main = defaultMain
  [ NonDet.benchmark
  , bgroup "WriterC"
    [ bgroup "Cod"
      [ bench "100"   $ whnf (run . runCod pure . execWriter @(Sum Int) . runCod pure . tellLoop) 100
      , bench "1000"  $ whnf (run . runCod pure . execWriter @(Sum Int) . runCod pure . tellLoop) 1000
      , bench "10000" $ whnf (run . runCod pure . execWriter @(Sum Int) . runCod pure . tellLoop) 10000
      ]
    , bgroup "standalone"
      [ bench "100"   $ whnf (run . execWriter @(Sum Int) . tellLoop) 100
      , bench "1000"  $ whnf (run . execWriter @(Sum Int) . tellLoop) 1000
      , bench "10000" $ whnf (run . execWriter @(Sum Int) . tellLoop) 10000
      ]
    ]
  ,
    bgroup "Strict StateC"
    [ bgroup "Cod"
      [ bench "100"   $ whnf (run . runCod pure . execState @(Sum Int) 0 . runCod pure . modLoop) 100
      , bench "1000"  $ whnf (run . runCod pure . execState @(Sum Int) 0 . runCod pure . modLoop) 1000
      , bench "10000" $ whnf (run . runCod pure . execState @(Sum Int) 0 . runCod pure . modLoop) 10000
      ]
    , bgroup "standalone"
      [ bench "100"   $ whnf (run . execState @(Sum Int) 0 . modLoop) 100
      , bench "1000"  $ whnf (run . execState @(Sum Int) 0 . modLoop) 1000
      , bench "10000" $ whnf (run . execState @(Sum Int) 0 . modLoop) 10000
      ]
    ]
  ,
    bgroup "InterpretC vs InterpretStateC vs StateC"
    [ bgroup "InterpretC"
      [ bench "100"   $ whnf (\n -> run $ evalState @(Sum Int) 0 $ runInterpret (\case { Get k -> get @(Sum Int) >>= k ; Put s k -> put s >> k }) $ modLoop n) 100
      , bench "1000"  $ whnf (\n -> run $ evalState @(Sum Int) 0 $ runInterpret (\case { Get k -> get @(Sum Int) >>= k ; Put s k -> put s >> k }) $ modLoop n) 1000
      , bench "10000" $ whnf (\n -> run $ evalState @(Sum Int) 0 $ runInterpret (\case { Get k -> get @(Sum Int) >>= k ; Put s k -> put s >> k }) $ modLoop n) 10000
      ]
    , bgroup "InterpretStateC"
      [ bench "100"   $ whnf (\n -> run $ runInterpretState (\ s -> \case { Get k -> runState @(Sum Int) s (k s) ; Put s k -> runState s k }) 0 $ modLoop n) 100
      , bench "1000"  $ whnf (\n -> run $ runInterpretState (\ s -> \case { Get k -> runState @(Sum Int) s (k s) ; Put s k -> runState s k }) 0 $ modLoop n) 1000
      , bench "10000" $ whnf (\n -> run $ runInterpretState (\ s -> \case { Get k -> runState @(Sum Int) s (k s) ; Put s k -> runState s k }) 0 $ modLoop n) 10000
      ]
    , bgroup "StateC"
      [ bench "100"   $ whnf (run . evalState @(Sum Int) 0 . modLoop) 100
      , bench "1000"  $ whnf (run . evalState @(Sum Int) 0 . modLoop) 1000
      , bench "10000" $ whnf (run . evalState @(Sum Int) 0 . modLoop) 10000
      ]
    ]
  ]

tellLoop :: Has (Writer (Sum Int)) sig m => Int -> m ()
tellLoop i = replicateM_ i (tell (Sum (1 :: Int)))

modLoop :: Has (State (Sum Int)) sig m => Int -> m ()
modLoop i = replicateM_ i (modify (+ (Sum (1 :: Int))))

newtype Cod m a = Cod { unCod :: forall b . (a -> m b) -> m b }
  deriving (Functor)

runCod :: (a -> m b) -> Cod m a -> m b
runCod = flip unCod

instance Applicative (Cod m) where
  pure a = Cod (\ k -> k a)
  (<*>) = ap

instance Monad (Cod m) where
  Cod a >>= f = Cod (\ k -> a (runCod k . f))

instance (Algebra sig m, Effect sig) => Algebra sig (Cod m) where
  eff op = Cod (\ k -> eff (handle (Identity ()) (runCod (pure . Identity) . runIdentity) op) >>= k . runIdentity)
