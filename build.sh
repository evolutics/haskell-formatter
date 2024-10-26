#!/bin/bash

set -o errexit -o nounset -o pipefail

readonly SCRIPT_FOLDER="$(dirname "$(readlink --canonicalize "$0")")"
cd "${SCRIPT_FOLDER}"
stack --system-ghc test
