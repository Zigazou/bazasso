#!/bin/bash

function find_first() {
    local directory="$1"
    local name="$2"

    find "$directory" -maxdepth 1 -type f -name "$2" -print \
        | head --lines=1
}
