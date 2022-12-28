#!/usr/bin/env bash

FWDIR="$(
  cd "$(dirname "$0")"/.. || exit
  pwd
)"

${FWDIR}/gradlew test "${@}"

cat **/build/reports/tests/test-xml/*.xml | grep -5 "<failure"
