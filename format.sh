#!/bin/bash

set -o errexit -o nounset -o pipefail

find Setup.hs src testsuite/src -name '*.hs' -type f -print0 \
  | xargs -0 -I {} -n 1 stack run -- --force --input {} --output {}
