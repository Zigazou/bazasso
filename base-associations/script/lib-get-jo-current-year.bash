#!/bin/bash

function get_jo_current_year() {
    local currentyear
    local parutions

    currentyear=$(wget --output-document=- "$1" 2>> "$LOGFILE" \
        | hxnormalize \
        | hxselect "a[href^=ASS_" \
        | hxwls -b "$1"
    ) || return 1

    if [ "$currentyear" == "" ]
    then
        >&2 printf "Lien introuvable\n"
        return 2
    fi

    parutions=$(wget --output-document=- "$currentyear" 2>> "$LOGFILE" \
        | hxnormalize \
        | hxselect "a[href^=ASS]" \
        | hxwls -b "$currentyear"
    ) || return 1

    if [ "$parutions" == "" ]
    then
        >&2 printf "Liens introuvables\n"
        return 2
    fi
        
    printf "%s" "$parutions"
}