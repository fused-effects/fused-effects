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
import qualified Reader.Base
import qualified Reader.Function
import qualified Reader.ReaderT
import qualified State.Lazy
import qualified State.StateT.Lazy
import qualified State.StateT.Strict
import qualified State.Strict
import qualified Writer.Strict
import qualified Writer.WriterT.Lazy
import qualified Writer.WriterT.Strict
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
  , Reader.Base.tests
  , Reader.Function.tests
  , Reader.ReaderT.tests
  , State.Lazy.tests
  , State.StateT.Lazy.tests
  , State.StateT.Strict.tests
  , State.Strict.tests
  , Writer.Strict.tests
  , Writer.WriterT.Lazy.tests
  , Writer.WriterT.Strict.tests
  ]
