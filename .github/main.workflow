workflow "Build" {
  on = "push"
  resolves = ["robrix/cabal-action@master"]
}

action "robrix/cabal-action@master" {
  uses = "robrix/cabal-action@master"
  args = "new-update && cabal new-build"
}
