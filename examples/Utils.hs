module Utils
( module Utils
) where

import Hedgehog

data TestTree
  = Leaf String Property
  | Branch String [TestTree]

checkTestTree :: TestTree -> IO Bool
checkTestTree t = case t of
  Leaf   n p  ->        putStrLn n  *> check p                   <* putStrLn ""
  Branch n ts -> and <$ putStrLn n <*> traverse checkTestTree ts <* putStrLn ""

testGroup :: String -> [TestTree] -> TestTree
testGroup = Branch

testProperty :: String -> Property -> TestTree
testProperty = Leaf
