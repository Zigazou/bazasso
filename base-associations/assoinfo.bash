#!/bin/bash

function info_asso() {
    printf "
        SELECT rnawaldec.titre
        FROM   rnawaldec
        WHERE  rnawaldec.id = '%s';
        " "$1"
}

waldec="$1"

info_asso | sqlite3 associations.db
