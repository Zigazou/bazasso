#!/bin/bash
function extraction_annonces_themes() {
    tidy -xml -quiet 2> /dev/null \
        | xslsproc script/annonces-themes-jo-to-csv.xsls -
}
