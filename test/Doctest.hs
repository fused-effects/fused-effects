module Main
( main
) where

import Control.Monad.Base
import System.Environment
import Test.DocTest
import Build_doctests (Component (..), components)
import Data.Foldable

main :: IO ()
main = for_ components $ \(Component name flags pkgs sources) -> do
  args <- getArgs
  let args = "--fast" : flags ++ pkgs ++ sources
  doctest args
