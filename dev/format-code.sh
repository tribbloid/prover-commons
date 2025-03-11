#!/usr/bin/env bash

FWDIR="$(cd "`dirname "$0"`"/..; pwd)"

cd "${FWDIR}" || exit

"${CRDIR}"/make-all.sh

"${FWDIR}"/gradlew scalafix -Dorg.gradle.parallel=false
 # consumes too much memory to run in parallel

scalafmt

"${FWDIR}"/gradlew scalafix -Dorg.gradle.parallel=false