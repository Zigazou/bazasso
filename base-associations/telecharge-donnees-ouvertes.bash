#!/bin/bash
source script/lib-assert.bash
source script/lib-exit-on-error.bash

source script/lib-get-region-link.bash
source script/lib-get-jo-current-year.bash
source script/lib-get-rna-links.bash
source script/lib-get-sirene-link.bash

LOGFILE="telecharge-donnees-ouvertes.log"
REGIONBASE="http://www.nosdonnees.fr/wiki/index.php/Fichier"
REGIONFILE="EUCircos_Regions_departements_circonscriptions_communes_gps.csv.gz"

JOANNONCE="https://echanges.dila.gouv.fr/OPENDATA/ASSOCIATIONS"

RNAPAGE="https://www.data.gouv.fr/fr/datasets/repertoire-national-des-associations/"

SIRENEPAGE="https://www.data.gouv.fr/fr/datasets/base-sirene-des-entreprises-et-de-leurs-etablissements-siren-siret"

# Requirements.
assert "Wget n'est pas installé" which wget
assert "html-xml-utils n'est pas installé" which hxselect

# Ensures source directory exists.
mkdir --parents source
cd source

if [ -f "$REGIONFILE" ]
then
    printf "Fichier des régions/départements déjà présent !\n"
else
    printf "Détermination de l'adresse du fichier des régions/départements..."
    REGIONURL=$(get_region_link "$REGIONBASE:$REGIONFILE" 2>> "$LOGFILE")
    exit_on_error

    printf "Récupération de %s..." "$REGIONURL"
    wget --no-clobber "$REGIONURL" 2>> "$LOGFILE"
    exit_on_error
fi

printf "Récupération du Journal Officiel de l'année en cours :\n"
get_jo_current_year "$JOANNONCE/" 2>> "$LOGFILE" | while read parution
do
    if [ -f $(basename "$parution") ]
    then
        printf "  - %s déjà récupéré !\n" "$parution"
    else
        printf "  - Récupération de %s..." "$parution"
        wget --no-clobber "$parution" 2>> "$LOGFILE"
        exit_on_error
    fi
done

printf "Récupération du Journal Officiel 2004-2017 :\n"
for year in {2004..2017}
do
    parutions="$JOANNONCE/FluxHistorique/ASS_${year}.tar.gz"
    parutionsfile=$(basename "$parutions")

    if [ -f "$parutionsfile" ]
    then
        printf "  - %s déjà récupéré !\n" "$parutions"
    else
        printf "  - Récupération de %s..." "$parutions"
        wget --no-clobber "$parutions" 2>> "$LOGFILE"
        exit_on_error
    fi

    printf "  - Décompression de %s..." "$parutionsfile"
    tar xzf "$parutionsfile" --skip-old-files 2>> "$LOGFILE"
    exit_on_error
done

printf "Récupération du Journal Officiel 1997-2004 :\n"
for year in {1997..2004}
do
    parutions="$JOANNONCE/FluxHistorique/stock_assoc_${year}.7z"

    if [ -f $(basename "$parutions") ]
    then
        printf "  - %s déjà récupéré !\n" "$parutions"
    else
        printf "  - Récupération de %s..." "$parutions"
        wget --no-clobber "$parutions" 2>> "$LOGFILE"
        exit_on_error
    fi
done

printf "Récupération des bases de données RNA :\n"
get_rna_links "$RNAPAGE" 2>> "$LOGFILE" | while read database
do
    printf "  - Récupération de %s..." "$database"
    wget --content-disposition --no-clobber "$database" 2>> "$LOGFILE"
    exit_on_error
done

printf "Récupération de la base Sirene :\n"
get_sirene_link "$SIRENEPAGE" 2>> "$LOGFILE" | while read database
do
    printf "  - Récupération de %s..." "$database"
    wget --content-disposition --no-clobber "$database" 2>> "$LOGFILE"
    exit_on_error
done