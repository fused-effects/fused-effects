module Main (main) where

import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.Process.Typed

main :: IO b
main = do
  print "Converting docs"
  let examplesDir = "examples"
      examples = ["ReinterpretingEffects", "ReinterpretLog"]
      docsDir = "docs/examples"
      mkHs x = examplesDir ++ "/" ++ x ++ ".hs"
      mkHsMd x = mkHs x ++ ".md"
      mkMd x = docsDir ++ "/" ++ x ++ ".md"
      moveDocs = foldMap (\x -> "mv " ++ mkHsMd x ++ " " ++ mkMd x ++ ";") examples
      mkFile x = "-f " ++ mkHs x
  s1 <- runProcess $ shell $ "lima hs2md " ++ foldMap (\x -> mkFile x ++ " ") examples
  when (s1 /= ExitSuccess) (print "Failed to convert the examples. Exiting ..." >> exitFailure)
  s2 <- runProcess $ shell moveDocs
  when (s2 /= ExitSuccess) (print "Failed to move the generated docs. Exiting ..." >> exitFailure)
  exitSuccess