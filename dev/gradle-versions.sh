#!/usr/bin/env bash

FWDIR="$(
  cd "$(dirname "$0")"/.. || exit
  pwd
)"

${FWDIR}/gradlew wrapper --gradle-version=8.10.1

${FWDIR}/gradlew dependencyUpdates
