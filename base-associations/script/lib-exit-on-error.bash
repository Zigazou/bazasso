#!/bin/bash

function exit_on_error() {
    if [ $? -ne 0 ]
    then
        exit 1
    else
        printf " OK\n"
    fi
}
