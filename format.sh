 #!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

find Setup.hs src testsuite/src -type f -name '*.hs' -print0 \
  | xargs -n 1 -0 -I {} stack run -- --force --input {} --output {}
