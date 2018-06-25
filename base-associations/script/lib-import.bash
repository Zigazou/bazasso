#!/bin/bash

function import() {
    local introduction="$1"
    local sourcefile="$2"
    local table="$3"

    printf "Import %s (table %s)..." "$introduction" "$table"
    import_file "$sourcefile" "$table" | sqlite3 associations.db
    exit_on_error
}
