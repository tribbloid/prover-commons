#!/usr/bin/env bash

FWDIR="$(
  cd "$(dirname "$0")"/.. || exit
  pwd
)"
CRDIR="$(
  cd "$(dirname "$0")" || exit
  pwd
)"
DATE=$(date +%Y-%m-%dT%H:%M:%S%z)

mkdir -p ${FWDIR}/logs/compile

${CRDIR}/make-all.sh --info > ${FWDIR}/logs/compile/"$DATE".log
