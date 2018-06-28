#!/bin/bash
function extraction_sirene() {
    tail --lines=+2 \
        | cut --delimiter=';' \
              --field=1,2,33,36,43,46,50,56,57,58,59,60,67,71,73,77,79,83,87,88
}
