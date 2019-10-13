module Main
( main
) where

import qualified Empty.Base
import qualified Empty.Maybe
import qualified Empty.MaybeT
import qualified Error
import qualified Fusion
import qualified NonDet.Church
import qualified Reader
import qualified State
import qualified Writer
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Empty.Base.tests
  , Empty.Maybe.tests
  , Empty.MaybeT.tests
  , Error.tests
  , Fusion.tests
  , NonDet.Church.tests
  , Reader.tests
  , State.tests
  , Writer.tests
  ]
