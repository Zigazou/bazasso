#!/bin/bash
function extraction_regions() {
    tail --lines=+2 \
        | cut --delimiter=';' --field=2,3,4 \
        | sort \
        | uniq
}
