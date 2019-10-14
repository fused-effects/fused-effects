module Main
( main
) where

import qualified Choose
import qualified Cull
import qualified Empty
import qualified Error
import qualified Fusion
import qualified NonDet
import qualified NonDet.Church
import qualified Reader
import qualified State
import qualified Writer
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Choose.tests
  , Cull.tests
  , Empty.tests
  , Error.tests
  , Fusion.tests
  , NonDet.tests
  , NonDet.Church.tests
  , Reader.tests
  , State.tests
  , Writer.tests
  ]
