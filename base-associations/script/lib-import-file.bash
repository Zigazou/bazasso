#!/bin/bash

function import_file() {
    # The source files use the semi-colon as delimiter.
    printf '.separator ";"\n'
    printf ".import %s %s\n" "$1" "$2"
}
