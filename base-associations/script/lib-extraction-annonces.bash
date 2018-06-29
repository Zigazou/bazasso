#!/bin/bash
function extraction_annonces() {
    tidy -xml -quiet 2> /dev/null \
        | xslsproc script/annonces-jo-to-csv.xsls -
}
