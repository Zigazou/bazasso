#!/bin/bash

function assert {
  # First parameter is the message in case the assertion is not verified
  local message="$1"

  # The remaining arguments make the command to execute
  shift

  # Run the command, $@ ensures arguments will remain in the same position.
  # "$@" is equivalent to "$1" "$2" "$3" etc.
  "$@" 2>&1 > /dev/null

  # Get the return code
  local rc=$?

  # If everything is okay, there's nothing left to do
  [ $rc -eq 0 ] && return 0

  # An error occured, retrieved the line and the name of the script where
  # it happend
  set $(caller)

  # Get the date and time at which the assertion occured
  local date=$(date "+%Y-%m-%d %T%z")

  # Output an error message on the standard error
  # Format: date script [pid]: message (linenumber, return code)
  echo "$date $2 [$$]: $message (line=$1, rc=$rc)" >&2

  # Exit with the return code of the assertion test
  exit $rc
}
