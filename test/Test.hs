module Main
( main
) where

import qualified Empty.Base
import qualified Empty.Maybe
import qualified Error.Base
import qualified Error.Either
import qualified Fusion
import qualified NonDet.Church
import qualified Reader.Base
import qualified Reader.Function
import qualified Reader.ReaderT
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Empty.Base.tests
  , Empty.Maybe.tests
  , Error.Base.tests
  , Error.Either.tests
  , Fusion.tests
  , NonDet.Church.tests
  , Reader.Base.tests
  , Reader.Function.tests
  , Reader.ReaderT.tests
  ]
