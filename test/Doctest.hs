module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  doctest ("-package random" : "-isrc" : "--fast" : if null args then ["src"] else args)
