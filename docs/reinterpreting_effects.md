# Reinterpreting effects

This article assumes you are already familiar with [defining effects and their handlers](https://github.com/fused-effects/fused-effects#defining-new-effects).

One of the nice aspects of effects is that they can support multiple effect handlers. Effects only specify actions, they don't actually perform them. Therefore, it's possible to "reinterpret" effects. There are multiple senses in which an effect can be reinterpreted:

- Implementing an effect in terms of other effects. "Stacking" effects is a powerful tool for cleanly dividing implementations into the relevant abstraction layers with minimal leakage of implementation details.
- Rewriting an effect and/or performing actions with the effect value and then performing the originally intended effect. This technique is conceptually similar to the middleware pattern commonly used in web applications.

Let's explore both of these effect interpretation strategies with a small motivating example:

> We would like to implement a client library for an [HTTP-based API that provides interesting cat facts](https://alexwohlbruck.github.io/cat-facts/docs/).

Let's break down some of the properties of the API client that would be desirable for a production use case:

- We would like to have our cat facts API be able to support different cat fact data sources in the future.
- We would like to be able to mock failure conditions (such as network connectivity issues) for testing purposes.
- We would like to be able to track timing metrics for how quickly we can retrieve cat facts.

### Initial setup

``` haskell
module CatFacts where
import Data.Aeson -- from the aeson package
import Data.Time
import qualified Network.HTTP.Client as HTTP -- from the http-client package
import qualified Network.HTTP.Client.TLS as HTTP -- from the http-client-tls package
```

Since one of the best parts about effects is being able to think at a domain language level,
let's start with defining the desired data which we wish to retrieve and an interface that
feels natural to work with:

``` haskell
-- | The basic fact that we will retrieve.
data CatFact = CatFact
  { catFact :: String
  } deriving (Show)

-- | Our high level effect type that will be able to target different data sources.
data CatFactClient m k
  = ListFacts Int {- ^ Number of facts to fetch -} ([CatFact] -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

listFacts :: Has CatFactClient sig m => Int -> m [CatFact]
listFacts n = send (ListFacts n pure)
```

Now that we have our very simple DSL in place, let's think about the underlying API: we know that it's JSON-based, so let's introduce the notion of a handler that is provided a request and hands back something that can be parsed out from a JSON document.

``` haskell
data JsonFetch req m k
  = forall a. (FromJSON a) => JsonFetch req (a -> m k)

fetchJson :: (Has (JsonFetch req) sig m, FromJSON a) => req -> m a
fetchJson r = send (JsonFetch r pure)
```

Let's introduce an effect type for making arbitrary requests over HTTP:

``` haskell
data Http m k
  = SendRequest HTTP.Request (HTTP.Response L.ByteString -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor, Effect)

sendRequest :: Has Http sig m => HTTP.Request -> m (HTTP.Response L.ByteString)
sendRequest r = send (SendRequest r pure)
```

As you can hopefully see, we've decomposed the original problem into several small effects that take care of a different layer of the original problem description. Now let's take a look at how we can compose them together to achieve our original goals.

## "Stacking" effects

### The production use-case

Now that we have these 3 mini-DSL types established, we need to stitch them together.

Let's take a top-down approach to the implementation again. We plan to fetch some JSON and convert it into a list of `CatFact`s from a public API.

``` haskell
catFactsEndpoint :: HTTP.Request
catFactsEndpoint = HTTP.parseRequest_ "https://cat-fact.herokuapp.com/facts"

instance (Has (JsonFetch HTTP.Request) sig m, Algebra sig m) => Algebra (CatFactClient :+: sig) (CatFactsApiC m) where
  eff (L (ListFacts numberOfFacts k)) = CatFactsApiC (fetchJson catFactsEndpoint >>= k)
  eff (R other) = CatFactsApiC (eff (handleCoercible other))
```

Now we need to support fetching JSON given an HTTP request. We have no guarantee that an arbitrary HTTP request
will actually return JSON, so for this implementation we have to account for failure conditions. This provides
a great opportunity to see how effect handlers can actually rely on *multiple underlying effects*!

We'll continue with the approach of remaining as fine-grained as possible.

We can conceive that many implementations of `JsonFetch` could return a malformed JSON response:

``` haskell
data JsonParseError = JsonParseError String
```

A more HTTP-centric issue is that we received a content type we can't use. In this case, anything that's not `application/json`:

``` haskell
data InvalidContentType = InvalidContentType String
```

Now we define a JSON effect handler that depends on _three_ underlying effects:

1. `Http` - we need to be able to make requests
2. `Throw JsonParseError` - we need to be able to signal that some aspect of the JSON wasn't what we expected.
3. `Throw InvalidContentType` - we need to be able to signal what we got wasn't JSON at all!

``` haskell

decodeOrThrow :: (Has (Throw JsonParseError) sig m, FromJSON a) => L.ByteString -> m a
decodeOrThrow = either (throwError . JsonParseError) pure . eitherDecode

instance (Has (Http :+: Throw JsonParseError :+: Throw InvalidContentType) sig m)
  => Algebra (JsonFetch HTTP.Request :+: sig) (JsonFetchHttpC m) where
  eff (L (JsonFetch req k)) = JsonFetchHttpC (do
    resp <- sendRequest req
    case lookup hContentType (HTTP.responseHeaders resp) of
      -- We'll assume that no specific content type equates to a JSON response.
      Nothing                 -> decodeOrThrow (HTTP.responseBody resp)
      Just "application/json" -> decodeOrThrow (HTTP.responseBody resp)
      Just other              -> throwError (InvalidContentType (show other)))
  eff (R other) = JsonFetchHttpC
```

The nice aspect of this is that we have neatly contained the failure scenarios to their relevant strata rather than leaking them into the higher-level abstraction!

Now we need to support performing HTTP requests:

```
instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (HttpC m) where
  eff (L (SendRequest req k)) =
      HttpC (liftIO (HTTP.getGlobalManager >>= HTTP.httpLbs req) >>= runHttp . k)
  eff (R other) = HttpC (eff (hmap runHttp other))
```

Note for the above code snippets how the `CatFactsApiC` carrier delegates fetching JSON to any other effect that supports retrieving JSON given an HTTP request specification.

Similarly, `JsonFetchHttpC` itself doesn't know how to perform an HTTP request. It delegates the request itself to a handler that implements the `Algebra` class for `(Http :+: sig)`.

Putting it all together for the actual production use case:

``` haskell
main :: IO ()
main = do
  catFacts <- runHttp (runJsonFetch (runCatFactsApi (listFacts 10)))
  mapM_ (putStrLn . catFact) catFacts
```

Produces:

```
```

### Testing with alternative effect handlers

This time let's go from the bottom up. In situations where IO is involved, failure scenarios tend
to surface from least-pure parts of code. In this case, we should therefore implement some facilities
to experiment with the most failure-prone area: the network itself.

``` haskell
newtype MockHttpC m a = MockHttpC { runMockHttpC :: ReaderC (HTTP.Request -> IO HTTP.Response) m a }

runMockHttp :: (HTTP.Request -> IO (HTTP.Response L.ByteString)) -> MockHttpC m a -> m a
runMockHttp responder m = runReader responder (runMockHttpC m)

instance (Algebra sig m) => Algebra (Http :+: sig) (MockHttpC m) where
  eff (L (SendRequest req k)) = MockHttpC ask >>= \responder -> liftIO (responder req) >>= (MockHttpC . k)
  eff (R other) = MockHttpC (eff handleCoercible other)

faultyNetwork :: HTTP.Request -> IO (HTTP.Response L.ByteString)
faultyNetwork req = const (throwIO (HTTP.HttpExceptionRequest req HTTP.ConnectionTimeout))

wrongContentType :: HTTP.Request -> IO (HTTP.Response L.ByteString)
wrongContentType req = pure
  where
    resp = HTTP.Response L.ByteString
      { responseStatus = ok200
      , responseVersion = http11
      , responseHeaders = [("Content-Type", "text/xml")]
      , responseBody = "[{\"text\": \"Cats are not dogs\"}]"
      , responseCookieJar = mempty
      , responseClose = pure ()
      }

badJson :: HTTP.Request -> IO (HTTP.Response L.ByteString)
badJson req = pure
  where
    resp = HTTP.Response L.ByteString
      { responseStatus = ok200
      , responseVersion = http11
      , responseHeaders = [("Content-Type", "application/json")]
      , responseBody = "{}"
      , responseCookieJar = mempty
      , responseClose = pure ()
      }
```

Those are probably the main failure scenarios. If for some reason we wanted to bypass the network stack entirely, we can also
provide a harness to test against arbitrary JSON values:

``` haskell

runJsonPure :: Value -> JsonRawC m a -> m a
runJsonPure = undefined

instance (Algebra sig m) => Algebra (JsonFetch req :+: sig) (JsonPure m) where
```

With effects, we have fine-grained ways of testing slices of our API. All that's needed
to turn an integration test into a unit test or vice versa is a different set of `Algebra`-implementing effect handlers!

## Observing & altering effects

Building new effect handling algebras can be a little bit verbose. In simpler situations, we may want to simply operate
on an effect without having to implement a whole new `Algebra` instance. We still have yet to build a solution to tracking
operational metrics (like request timings), so let's look at how to build a sort of "effect middleware" using `InterpretC`.

`InterpretC` is an effect carrier that is intended for prototyping new effects that passes a callback function each
occurence of the specified effect type that is called via `send`. One trick that can be useful is to intercept an effect,
operate on the effect, and then re-`send` the effect. In other words, it's perfectly valid to have multiple handlers
for the same effect type and dispatch to the ones higher in the effect stack! Let's use this approach to time and log
our HTTP requests:

``` haskell
traceHttp
  :: (Has Http sig m, MonadIO m)
  => (forall s. Reifies s (Handler Http m) => InterpretC s Http m a)
  -> m a
traceHttp = runInterpret $ \r@(SendRequest req sendResp) -> do
  startTime <- liftIO getCurrentTime
  liftIO (putStr (show (HTTP.path req) ++ " ... "))
  -- Pass the request on to something that actually knows how to respond.
  send $ SendRequest req $ \resp -> do
    -- Once the actual response is obtained,
    -- we can capture the end time and status of the response.
    endTime <- liftIO getCurrentTime
    let timeSpent = startTime `diffTime` endTime
    putStrLn ("[" ++ show (HTTP.responseStatus resp) ++ "] took " ++ )
    sendResp resp
```
