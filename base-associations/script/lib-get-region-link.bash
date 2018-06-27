#!/bin/bash

function get_region_link() {
    local link

    link=$(
        wget --output-document=- "$1" 2>> "$LOGFILE" \
            | hxselect -i "a.internal" \
            | hxwls -b "$1"
    ) || return 1

    if [ "$link" == "" ]
    then
        >&2 printf "Lien introuvable\n"
        return 2
    fi

    printf "%s" "$link"
    return 0
}
