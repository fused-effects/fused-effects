# FAQs

## Why is `Algebra` called `Algebra`, and not something more specific to the interpretation of effects?

In previous versions of `fused-effects`, `Algebra` was called Carrier. The authors chose to rename this to keep it in line with the literature (the corresponding typeclass is called `TermAlgebra` in _Fusion for Free_), emphasize the importance of morphisms over objects, and emphasize its similarity to the common Haskell idiom of [F-algebras](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras). The term “algebra” stems from the Arabic جبر, _jabr_, which roughly translates to “reunion” or “restoration”. This propery is visible in the definition of the `Carrier` class’s `eff` method:

```haskell
eff :: sig m a -> m a
```

Like the traditional encoding of F-algebras (`f a -> a`), this describes a function that reunites an effect signature `sig` with its monadic context `m`.


## When do I need to use the type application (`@Foo`) syntax?

Because a given effectful operation can have multiple `State` or `Reader` effects, your code may fail to typecheck if it invokes an ambiguous state or reader effect, such as follows:

``` haskell
ambig :: (Has (State Int) sig m, Has (State Float) sig m, MonadIO m) => m ()
ambig = do
  item <- get
  liftIO . putStrLn $ "got item: " <> show item
```

Because the `item` variable is not annotated with a concrete type, GHC will try to infer which we you meant. In this case, it is unable to, as `item` is passed to the polymorphic `show` function. Because both `Int` and `Float` values can be passed to `show`, GHC will reject this program with an error relating to ambiguous types. The `-XTypeApplications` extension to GHC provides a syntactically clean way to specify which type we meant:

```haskell
okay :: (Has (State Int) sig m, Has (State Float) sig m, MonadIO m) => m ()
okay = do
  item <- get @Int
  liftIO . putStrLn $ "got item: " <> show item
```

The `@Int` syntax—an _explicit type application_ specifies that the return type of `get` must in this case be an `Int`. For more information about type applications, consult the [GHC manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications).


## How can I build effect stacks that interoperate correctly with `mtl`?

There are two approaches: the first is to use the monadic types defined by `transformers` as the carriers for your effects. The resulting composition of monads will interoperate with `mtl` and any `mtl`-compatible library. The second is to wrap an existing monad stack with a phantom type representing some relevant effect information:

```haskell
newtype Wrapper s m a = Wrapper { runWrapper :: m a }
  deriving (Algebra sig, Applicative, Functor, Monad)

getState :: Has (State s) sig m => Wrapper s m s
getState = get
```

Indeed, `Wrapper` can now be made an instance of `MonadState`:

```haskell
instance Has (State s) sig m => MTL.MonadState s (Wrapper s m) where
  get = Control.Carrier.State.Strict.get
  put = Control.Carrier.State.Strict.put
```
