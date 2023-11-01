#!/usr/bin/env bash

CRDIR="$(
  cd "$(dirname "$0")" || exit
  pwd
)"

ARGS=("-PsplainVersion=1.0.3" "${@}")

exec "${CRDIR}"/CI/main.sh "${ARGS[@]}"
