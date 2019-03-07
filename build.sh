#!/bin/sh
#
# Runs cabal (intended to be used in by the Action defined in Dockerfile)

set -eux

cabal new-update

sh -c "cabal $*"
