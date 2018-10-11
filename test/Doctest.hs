module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  doctest ("-isrc" : "--fast" : if null args then ["src"] else args)
