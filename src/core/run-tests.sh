#!/usr/bin/env bash

function execute_test {
  echo
  set -x
  nim c -r $@
  if [[ $? != 0 ]]; then exit $?; fi
  set +x
}

execute_test corecommon
execute_test -d:useFloat64 corecommon

execute_test geometry
execute_test -d:useFloat64 geometry

execute_test interaction
execute_test -d:useFloat64 interaction

execute_test shape
execute_test -d:useFloat64 shape

execute_test transform
execute_test -d:useFloat64 transform

