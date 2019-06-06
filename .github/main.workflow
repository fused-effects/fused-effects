workflow "Build fused-effects" {
  on = "push"
  resolves = ["Build"]
}


action "Build" {
  uses = "./"
  args = "new-build --help"
}
