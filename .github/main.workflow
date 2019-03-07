workflow "Build fused-effects" {
  on = "push"
  resolves = [
    "Test",
  ]
}

action "Build" {
  uses = "./"
  args = "new-build"
}

action "Test" {
  uses = "./"
  args = "new-test"
  needs = ["Build"]
}
