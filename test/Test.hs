module Main
( main
) where

import qualified Choose
import qualified Cull
import qualified Cut
import qualified Cut.Church
import qualified Empty
import qualified Error
import qualified Fresh
import qualified Fusion
import qualified Lift
import qualified NonDet
import qualified NonDet.Church
import qualified Reader
import qualified State
import qualified Throw
import qualified Writer
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Choose.tests
  , Cull.tests
  , Cut.tests
  , Cut.Church.tests
  , Empty.tests
  , Error.tests
  , Fresh.tests
  , Fusion.tests
  , Lift.tests
  , NonDet.tests
  , NonDet.Church.tests
  , Reader.tests
  , State.tests
  , Throw.tests
  , Writer.tests
  ]
