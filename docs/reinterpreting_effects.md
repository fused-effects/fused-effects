# Reinterpreting effects

This article assumes you are already familiar with [defining effects and their handlers](defining_effects.md).

One of the nice aspects of effects is that they can support multiple effect handlers. Effects only specify actions, they don't actually perform them. Therefore, it's possible to "reinterpret" effects. There are multiple senses in which an effect can be reinterpreted:

- Implementing an effect in terms of other effects. "Reinterpreting" effects is a powerful tool for cleanly dividing implementations into the relevant abstraction layers with minimal leakage of implementation details.
- Rewriting an effect and/or performing actions with the effect value and then performing the originally intended effect. This technique is conceptually similar to the middleware pattern commonly used in web applications. This known as *interposition* (see works by Oleg Kiselyov et al.)

Let's explore both of these effect interpretation strategies with a small motivating example:

✨ We would like to implement a client library for an [HTTP-based API that provides interesting cat facts](https://alexwohlbruck.github.io/cat-facts/docs/). ✨

Let's break down some of the properties of the API client that would be desirable for a production use case:

1. We would like to have our cat facts API be able to support different cat fact data sources in the future.
2. We would like to be able to mock failure conditions (such as network connectivity issues) for testing purposes.
3. We would like to be able to track timing metrics for how quickly we can retrieve cat facts.

### Initial setup

``` haskell
{-# LANGUAGE ExistentialQuantification, DeriveFunctor,
DeriveGeneric, FlexibleContexts, FlexibleInstances,
GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,
RankNTypes, TypeApplications, TypeOperators, UndecidableInstances #-}
module CatFacts
    ( main
    ) where
-- from base
import Control.Applicative
import Control.Exception (throwIO)
import GHC.Generics (Generic1)
-- from fused-effects
import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.Error.Either
import Control.Carrier.Interpret
-- from transformers
import Control.Monad.IO.Class
-- From aeson
import Data.Aeson
-- From bytestring
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
-- From time
import Data.Time.Clock
-- From http-client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
-- From http-client-tls
import qualified Network.HTTP.Client.TLS as HTTP
-- From http-status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
```

Since one of the best parts about effects is being able to think at a domain language level,let's start with defining the desired data which we wish to retrieve and an interface that feels natural to work with:

``` haskell
-- | The basic fact that we will retrieve.
newtype CatFact = CatFact
  { catFact :: String
  } deriving (Show)

instance FromJSON CatFact where
  parseJSON = withObject "CatFact" (\o -> CatFact <$> o .: "text")

-- | Our high level effect type that will be able to target different data sources.
data CatFactClient m k
  = ListFacts Int {- ^ Number of facts to fetch -} ([CatFact] -> m k)
  deriving (Functor, Generic1)
  
instance Effect CatFactsClient

listFacts :: Has CatFactClient sig m => Int -> m [CatFact]
listFacts n = send (ListFacts n pure)
```

Now that we have our very simple DSL in place, let's think about the underlying API: we know that it's an HTTP-based system, so let's introduce the notion of a handler that is provided a request and hands back an HTTP response.

``` haskell
data Http m k
  = SendRequest HTTP.Request (HTTP.Response L.ByteString -> m k)
  deriving (Functor, Generic1)

instance Effect Http

sendRequest :: Has Http sig m => HTTP.Request -> m (HTTP.Response L.ByteString)
sendRequest r = send (SendRequest r pure)
```

The `listFacts` function provides the ‘what’ of this API, and the `sendRequest` function provides the ‘how’. In decomposing this problem into a set of effects, each responsible for a single layer of the original problem description, we provide ourselves with a flexible, composable vocabulary rather than a single monolithic action.

## "Stacking" effects

### The production use-case

Now that we have these two mini-DSL effect types established, we need to stitch them together.

Let's take a moment to think about what could go wrong with an HTTP API from which we plan to fetch some JSON and convert it into a list of `CatFact`s.

We can conceive that the server might occasionally return a malformed JSON response:

``` haskell
newtype JsonParseError = JsonParseError String
  deriving (Show, Eq)

decodeOrThrow :: (Has (Throw JsonParseError) sig m, FromJSON a) => L.ByteString -> m a
decodeOrThrow = either (throwError . JsonParseError) pure . eitherDecode
```

A more HTTP-centric issue is that we might receive a content type we can't use. In this case, anything that's not `application/json`:

``` haskell
newtype InvalidContentType = InvalidContentType String
  deriving (Show, Eq)
```

Now we need to support fetching JSON given an HTTP request. We have no guarantee that an arbitrary HTTP request will actually return JSON, so for this implementation we have to account for failure conditions. This provides a great opportunity to see how effect handlers can actually rely on *multiple underlying effects*!

``` haskell
newtype CatFactsApi m a = CatFactsApi { runCatFactsApi :: m a }
 deriving
   ( Monad
   , Functor
   , Applicative
   , MonadIO
   , Alternative
   )

catFactsEndpoint :: HTTP.Request
catFactsEndpoint = HTTP.parseRequest_ "https://cat-fact.herokuapp.com/facts/random"

instance ( Has Http sig m
         , Has (Throw JsonParseError) sig m
         , Has (Throw InvalidContentType) sig m
         , Algebra sig m
         ) =>
         Algebra (CatFactClient :+: sig) (CatFactsApi m) where
  alg (L (ListFacts numberOfFacts k)) = do
    resp <- sendRequest (catFactsEndpoint { HTTP.queryString = "?amount=" <> B.pack (show numberOfFacts) })
    case lookup hContentType (HTTP.responseHeaders resp) of
      Just "application/json; charset=utf-8" -> decodeOrThrow (HTTP.responseBody resp) >>= k
      other -> throwError (InvalidContentType (show other))
  alg (R other) = CatFactsApi (handleCoercible other)
```

We implement a `CatFacts` effect handler that depends on _three_ underlying effects:

1. `Http` - we need to be able to make requests
2. `Throw JsonParseError` - we need to be able to signal that some aspect of the JSON wasn't what we expected.
3. `Throw InvalidContentType` - we need to be able to signal what we got wasn't JSON at all!

The nice aspect of this is that we have neatly contained the failure scenarios to their relevant strata rather than leaking them into the higher-level abstraction (`listFacts`)!

Now we need to support performing HTTP requests:

``` haskell
newtype HttpClient m a = HttpClient { runHttp :: m a }
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadIO
    , Alternative
    )

instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (HttpClient m) where
  alg (L (SendRequest req k)) = liftIO (HTTP.getGlobalManager >>= HTTP.httpLbs req) >>= k
  alg (R other) = HttpClient (handleCoercible other)
```

Note for the above code snippets how the `CatFactsApi` carrier delegates fetching JSON to any other effect that supports retrieving JSON given an HTTP request specification.

Note as well that `CatFactsApi` itself doesn't know how to perform an HTTP request. It delegates the request itself to a handler that implements the `Algebra` class for `(Http :+: sig)`.

Putting it all together for the actual production use case:

``` haskell
handlePrint :: Either InvalidContentType (Either JsonParseError [CatFact]) -> IO ()
handlePrint r =
  case r of
    Left invalidContentTypeError -> print invalidContentTypeError
    Right ok -> case ok of
      Left jsonParseError -> print jsonParseError
      Right facts -> traverse (putStrLn . catFact) facts

catFactsRunner :: (Effect sig, Has Http sig m) => m (Either InvalidContentType (Either JsonParseError [CatFact]))
catFactsRunner =
  runError @InvalidContentType $
  runError @JsonParseError $
  runCatFactsApi $
  listFacts 10

main :: IO ()
main = runHttp catFactsRunner >>= handlePrint
```

Produces:

```
The Bengal is the result of crossbreeding between domestic cats and Asian leopard cats, and its name is derived from the scientific name for the Asian leopard cat (Felis bengalensis).
A happy cat holds her tail high and steady.
Kittens remain with their mother till the age of 9 weeks.
Recent studies have shown that cats can see blue and green. There is disagreement as to whether they can see red.
A steady diet of dog food may cause blindness in your cat - it lacks taurine.
Cat owners are 25% likely to pick George Harrison as their favorite Beatle.
The catnip plant contains an oil called hepetalactone which does for cats what marijuana does to some people. Not all cats react to it those that do appear to enter a trancelike state. A positive reaction takes the form of the cat sniffing the catnip, then licking, biting, chewing it, rub & rolling on it repeatedly, purring, meowing & even leaping in the air.
The color of the points in Siamese cats is heat related. Cool areas are darker.
Cats have free-floating clavicle bones that attach their shoulders to their forelimbs, which allows them to squeeze through very small spaces.
Wikipedia has a recording of a cat meowing, because why not?
```

### Testing with alternative effect handlers

Per point 2. of our initial implementation criteria, we want to be able to simulate failure cases for testing purposes. This is a great case for swapping in an alternative effect handler for our HTTP layer.

This time let's go from the bottom up. In situations where IO is involved, failure scenarios tend to surface from least-pure parts of code. In this case, we should therefore implement some facilities to experiment with the most failure-prone area: the network itself.

``` haskell
newtype MockHttpClient m a = MockHttpClient { runMockHttpClient :: ReaderC (HTTP.Request -> IO (HTTP.Response L.ByteString)) m a }
  deriving
   ( Monad
   , Functor
   , Applicative
   , MonadIO
   , Alternative
   )

runMockHttp :: (HTTP.Request -> IO (HTTP.Response L.ByteString)) -> MockHttpC m a -> m a
runMockHttp responder m = runReader responder (runMockHttpClient m)

instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (MockHttpClient m) where
  alg (L (SendRequest req k)) = MockHttpClient ask >>= \responder -> liftIO (responder req) >>= k
  alg (R other) = MockHttpClient (handleCoercible other)

faultyNetwork :: HTTP.Request -> IO (HTTP.Response L.ByteString)
faultyNetwork req = throwIO (HTTP.HttpExceptionRequest req HTTP.ConnectionTimeout)

wrongContentType :: HTTP.Request -> IO (HTTP.Response L.ByteString)
wrongContentType req = pure resp
  where
    resp = Response
      { responseStatus = ok200
      , responseVersion = http11
      , responseHeaders = [("Content-Type", "text/xml")]
      , responseBody = "[{\"text\": \"Cats are not dogs\"}]"
      , responseCookieJar = mempty
      , responseClose' = ResponseClose (pure ())
      }

badJson :: HTTP.Request -> IO (HTTP.Response L.ByteString)
badJson req = pure Response
  { responseStatus = ok200
  , responseVersion = http11
  , responseHeaders = [("Content-Type", "application/json; charset=utf-8")]
  , responseBody = "{}"
  , responseCookieJar = mempty
  , responseClose' = ResponseClose (pure ())
  }
```

Let's update our `main` function and watch it in action:

``` haskell
main :: IO ()
main = do
  -- Should return JsonParseError
  runMockHttp badJson catFactsRunner >>= handlePrint
  -- Should return InvalidContentType
  runMockHttp wrongContentType catFactsRunner >>= handlePrint
```

Which returns:

``` haskell
JsonParseError "Error in $: parsing [] failed, expected Array, but encountered Object"
InvalidContentType "Just \"text/xml\""
```

With effects, we have fine-grained ways of testing slices of our API. All that's needed to turn an integration test into a unit test or vice versa is a different set of `Algebra`-implementing effect handlers!

## Observing & altering effects

Building new effect handling algebras can be a little bit verbose. In simpler situations, we may want to simply operate
on an effect without having to implement a whole new `Algebra` instance. We still have yet to build a solution to tracking
operational metrics (like request timings), so let's look at how to build a sort of "effect middleware" using `InterpretC`.

`InterpretC` is an effect carrier that is intended for prototyping new effects that passes a callback function each
occurence of the specified effect type that is called via `send`. One trick that can be useful is to intercept an effect,
operate on the effect, and then re-`send` the effect (a.k.a. interposition). In other words, it's perfectly valid to have multiple handlers
for the same effect type and dispatch to the ones higher in the effect stack! Let's use this approach to time and log
our HTTP requests:

``` haskell
traceHttp
  :: (Has Http sig m, MonadIO m)
  => (forall s. Reifies s (Handler Http m) => InterpretC s Http m a)
  -> m a
traceHttp = runInterpret $ \r@(SendRequest req sendResp) -> do
  startTime <- liftIO getCurrentTime
  liftIO (putStr (B.unpack (HTTP.path req) ++ " ... "))
  -- Pass the request on to something that actually knows how to respond.
  send $ SendRequest req $ \resp -> do
    -- Once the actual response is obtained,
    -- we can capture the end time and status of the response.
    endTime <- liftIO getCurrentTime
    let timeSpent = endTime `diffUTCTime` startTime
    liftIO $ putStrLn ("[" ++ show (statusCode $ HTTP.responseStatus resp) ++ "] took " ++ show timeSpent ++ "\n\n")
    sendResp resp
```

Updating our `main` function once more:

``` haskell
main :: IO ()
main = runHttp (traceHttp catFactsRunner) >>= handlePrint
```

Returns:

```
/facts/random ... [200] took 0.979107082s


Cats have a special scent organ located in the roof of their mouth, called the Jacobson's organ. It analyzes smells - and is the reason why you will sometimes see your cat "sneer" (called the flehmen response or flehming) when they encounter a strong odor.
It's important for cats to have regular ear exams—this is something you can do at home! Gently fold back the ears and look into the ear canal. The inner ear should be pale pink with little to no earwax. If you notice redness, swelling, discharge, or a lot of earwax, it's time to see a veterinarian.
Siamese kittens are born white because of the heat inside the mother's uterus before birth. This heat keeps the kittens' hair from darkening on the points.
Declawing a cat is the same as cutting a human's fingers off at the knuckle. There are several alternatives to a complete declawing, including trimming or a less radical (though more involved) surgery to remove the claws. Instead, train your cat to use a scratching post.
There is a species of cat smaller than the average housecat. It is native to Africa and it is the Black-footed cat (Felis nigripes). Its top weight is 5.5 pounds.
Gatos.
Cats are the most interesting mammals on earth.
Cats have free-floating clavicle bones that attach their shoulders to their forelimbs, which allows them to squeeze through very small spaces.
Fossil records from two million years ago show evidence of jaguars.
Since cats are so good at hiding illness, even a single instance of a symptom should be taken very seriously.
```

### Conclusion

Reviewing our initial criteria, we have an eminently extensible system that lets us maintain a healthy separation of concerns–
All while still allowing non-invasive behavior changes through the ability to intercept, rewrite, and resend effects!

- [x] We would like to have our cat facts API be able to support different cat fact data sources in the future.
- [x] We would like to be able to mock failure conditions (such as network connectivity issues) for testing purposes.
- [x] We would like to be able to track timing metrics for how quickly we can retrieve cat facts.

![Mission accomplished](https://media.giphy.com/media/l6Td5sKDNmDGU/giphy.gif)
