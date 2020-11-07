#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

readonly SCRIPT_FOLDER="$(dirname "$(readlink --canonicalize "$0")")"

pushd "${SCRIPT_FOLDER}"

stack --system-ghc test

popd
