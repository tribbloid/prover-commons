#!/usr/bin/env bash

# TODO: replaced with compile + test_only?

FWDIR="$(
  cd "$(dirname "$0")"/.. || exit
  pwd
)"

echo "[COMPILING]" && \
"${FWDIR}"/make-all.sh "${@}" && \
echo "[RUNNING TESTS]" && \
"${FWDIR}"/test.sh "${@}"
