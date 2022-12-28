#!/usr/bin/env bash

FWDIR="$(
  cd "$(dirname "$0")"/.. || exit
  pwd
)"

${FWDIR}/gradlew test "${@}"

cat **/build/test-results/**/*.xml| grep -5 "<failure" || \
  echo "No test failure found"
