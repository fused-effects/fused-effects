# FAQs

## Why is `Algebra` called `Algebra`, and not something more specific to the interpretation of effects?

In previous versions of `fused-effects`, `Algebra` was called Carrier. The authors chose to rename this to keep it in line with the literature (the corresponding typeclass is called `TermAlgebra` in _Fusion for Free_), emphasize the importance of morphisms over objects, and emphasize its similarity to the common Haskell idiom of [F-algebras](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras). The term “algebra” stems from the Arabic جبر, _jabr_, which roughly translates to “reunion” or “restoration”. This propery is visible in the definition of the `Carrier` class’s `eff` method:

```haskell
eff :: sig m a -> m a
```

Like the traditional encoding of F-algebras (`f a -> a`), this describes a function that reunites an effect signature `sig` with its monadic context `m`.
