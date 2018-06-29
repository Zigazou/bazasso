#!/bin/bash
function extraction_sirene() {
    tail --lines=+2 \
        | csvcut ';' 0 1 32 35 42 45 49 55 56 57 58 59 66 70 72 76 78 82 86 87
}
