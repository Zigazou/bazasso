#!/bin/bash

function extraction_departements() {
    tail --lines=+2 \
        | cut --delimiter=';' --field=5,6,7 \
        | sort \
        | uniq
}
