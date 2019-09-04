module Main
( main
) where

import System.Environment
import Test.DocTest

main :: IO ()
main = getArgs >>= doctest . build where
  build args
    = "-isrc"
    : "--fast"
    : "-XFlexibleContexts"
    : "-XFlexibleInstances"
    : "-XTypeApplications"
    : if null args then ["src"] else args
