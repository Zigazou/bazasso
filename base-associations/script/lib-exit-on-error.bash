#!/bin/bash

function exit_on_error() {
    if [ $? -ne 0 ]
    then
        printf " Erreur\n"
        exit 1
    else
        printf " OK\n"
    fi
}
