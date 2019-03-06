workflow "Build fused-effects" {
  on = "push"
  resolves = [
    "Test",
    "Haddock",
    "Benchmark",
  ]
}

action "Build" {
  uses = "robrix/cabal-action@master"
  args = "new-update && cabal new-build"
}

action "Test" {
  uses = "robrix/cabal-action@master"
  args = "new-test"
  needs = ["Build"]
}

action "Haddock" {
  uses = "robrix/cabal-action@master"
  needs = ["Build"]
  args = "new-haddock"
}

action "Benchmark" {
  uses = "robrix/cabal-action@master"
  needs = ["Build"]
  args = "new-build benchmark"
}
