workflow "Build fused-effects" {
  on = "push"
  resolves = ["Build (not really)"]
}

action "Build (not really)" {
  uses = "./"
  args = "new-build --help"
}
