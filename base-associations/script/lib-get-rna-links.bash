#!/bin/bash

function get_rna_links() {
    local links
    
    links=$(
        wget --output-document=- "$1" 2>> "$LOGFILE" \
            | grep -o '<a[^>]*download[^>]*>' \
            | sed 's/@click.stop//g' \
            | hxwls -b "$1" \
            | head --lines=2
    ) || return 1

    if [ "$links" == "" ]
    then
        >&2 printf "Liens introuvables\n"
        return 2
    fi

    printf "%s\n" "$links"
    return 0
}
