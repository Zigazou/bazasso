#!/bin/bash

function get_sirene_link() {
    local link
    
    link=$(
        wget --output-document=- "$1" 2>> "$LOGFILE" \
            | grep -o '<a[^>]*download[^>]*>' \
            | sed 's/@click.stop//g' \
            | hxwls -b "$1" \
            | head --lines=1
    ) || return 1

    if [ "$link" == "" ]
    then
        >&2 printf "Lien introuvable\n"
        return 2
    fi

    printf "%s\n" "$link"
    return 0
}
