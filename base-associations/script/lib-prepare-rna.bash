#!/bin/bash

function prepare_rna() {
    # Removes the first row of a stream and converts it from UTF-8 to
    # ISO-8859-15.
    tail --lines=+2 | iconv --from-code="ISO-8859-15" --to-code="UTF-8"
}
