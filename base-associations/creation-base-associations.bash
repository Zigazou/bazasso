#!/bin/bash
source script/lib-assert.bash
source script/lib-exit-on-error.bash
source script/lib-import-file.bash
source script/lib-prepare-rna.bash
source script/lib-extraction-regions.bash
source script/lib-extraction-departements.bash
source script/lib-extraction-annonces.bash
source script/lib-extraction-annonces-themes.bash
source script/lib-import.bash
source script/lib-find-first.bash
source script/lib-affiche-aide.bash

# Display help if needed
[ "$1" != "" ] && affiche_aide

# Find files to import in the source directory
RNAIMPORT=$(find_first source "rna_import_????????.zip")
RNAWALDEC=$(find_first source "rna_waldec_????????.zip")
EUCIRCOS=$(find_first source "EUCircos_*.csv.gz")

# Requirements
assert "Python3 n'est pas installé" which python3
assert "zcat n'est pas installé" which zcat
assert "unzip n'est pas installé" which unzip
assert "SQLite3 n'est pas installé" which sqlite3
assert "iconv n'est pas installé" which iconv
assert "XSLTproc n'est pas installé" which xsltproc
assert "XSLSproc n'est pas installé" which xslsproc
assert "associations.db existe déjà" test ! -f associations.db
assert "script/association-tables.sql introuvable" \
    test -f script/association-tables.sql
assert "aucun fichier eucircos* trouvé" test "$EUCIRCOS" != ""
assert "aucun fichier source/rna_waldec_AAAAMMJJ.csv trouvé"
    test "$RNAIMPORT" != ""
assert "aucun fichier source/rna_import_AAAAMMJJ.csv trouvé" \
    test "$RNAWALDEC" != ""

# Creating the tables before importing allows to select the right field types
# and limits.
printf "Préparation de la base de données..."
sqlite3 associations.db < script/association-tables.sql
exit_on_error

# When importing CSV file in an already existing table, the first row is
# considered data not header. It must be removed. SQLite3 works natively with
# UTF-8 files, but the source files are encoded in ISO-8859-15.
printf "Préparation des associations historiques..."
unzip -p "$RNAIMPORT" | prepare_rna > temp/rna_import.csv
exit_on_error

printf "Préparation des associations Waldec..."
unzip -p "$RNAWALDEC" | prepare_rna > temp/rna_waldec.csv
exit_on_error

printf "Préparation des régions..."
zcat "$EUCIRCOS" | extraction_regions > temp/regions.csv
exit_on_error

printf "Préparation des départements..."
zcat "$EUCIRCOS" | extraction_departements > temp/departements.csv
exit_on_error

printf "Préparation des communes..."
zcat "$EUCIRCOS" | python3 script/extraction-communes.py > temp/communes.csv
exit_on_error

printf "Préparation des annonces du JO:\n"
for jo in source/ASS*.taz
do
    printf "  - Lecture de %s..." "$jo"

    tar xzf "$jo" --wildcards --to-stdout "*.xml" \
        | extraction_annonces \
        >> temp/annonces_jo.csv

    tar xzf "$jo" --wildcards --to-stdout "*.xml" \
        | extraction_annonces_themes \
        >> temp/annonces_themes_jo.csv

    exit_on_error
done

# Imports.
import "des régions" temp/regions.csv "region"
import "des départements" temp/departements.csv "departement"
import "des communes" temp/communes.csv "commune"
import "des associations historiques" temp/rna_import.csv "rnaimport"
import "des associations Waldec" temp/rna_waldec.csv "rnawaldec"
import "des annonces du JO" temp/annonces_jo.csv "joannonce"
import "des thèmes d'annonces du JO" temp/annonces_themes_jo.csv "joanntheme"

# Remove temporary files.
printf "Nettoyage des fichiers temporaires..."
rm --force temp/*
exit_on_error

