module Main
( main
) where

import qualified Empty.Base
import qualified Empty.Maybe
import qualified Empty.MaybeT
import qualified Error.Base
import qualified Error.Either
import qualified Error.ExceptT
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
  , Error.Base.tests
  , Error.Either.tests
  , Error.ExceptT.tests
  , Fusion.tests
  , NonDet.Church.tests
  , Reader.tests
  , State.tests
  , Writer.tests
  ]
