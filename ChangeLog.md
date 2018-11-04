- Adds a `Cut` effect which adds committed choice to nondeterminism.
- Adds a `Cull` effect which adds pruning to nondeterminism.
- Adds an example of using `NonDet`, `Cut`, and a character parser effect to define parsers.
- Changes `runNonDetOnce` to `runNonDetCull`, redefines it in terms of `runCull` and `runNonDet`, and removes `OnceC`.
- Fixes the table of contents links in the README.

# 0.1.1.0

- Adds a `runNonDetOnce` handler which terminates immediately upon finding a solution.

# 0.1.0.0

Initial release.
