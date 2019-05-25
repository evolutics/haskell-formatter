#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

readonly SCRIPT_FOLDER="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

pushd "${SCRIPT_FOLDER}"

stack --system-ghc test

popd
