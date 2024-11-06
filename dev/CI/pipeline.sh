#!/usr/bin/env bash

# TODO: replaced with compile + test_only?

FWDIR="$(cd "`dirname "$0"`"/..; pwd)"

echo "[COMPILING]" && \
"${FWDIR}"/make-all.sh "${@}" && \
echo "[RUNNING TESTS]" && \
"${FWDIR}"/test.sh "${@}"
