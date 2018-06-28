{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.GeneralTheme
    ( GeneralTheme(..)
    , generalThemes
    , gtOptionList
    ) where

import Import

data GeneralTheme = GeneralTheme
        { gtStart :: Text
        , gtName :: Text
        }

generalThemes :: [GeneralTheme]
generalThemes =
    [ GeneralTheme "001" "activités politiques"
    , GeneralTheme "002" "clubs, cercles de réflexion"
    , GeneralTheme "003" "défense de droits fondamentaux, activités civiques"
    , GeneralTheme "004" "justice"
    , GeneralTheme "005" "information communication"
    , GeneralTheme "006" "culture, pratiques d’activités artistiques, culturelles"
    , GeneralTheme "007" "clubs de loisirs, relations"
    , GeneralTheme "009" "action socio-culturelle"
    , GeneralTheme "010" "préservation du patrimoine"
    , GeneralTheme "011" "sports, activités de plein air"
    , GeneralTheme "013" "chasse pêche"
    , GeneralTheme "014" "amicales, groupements affinitaires, groupements d'entraide (hors défense de droits fondamentaux"
    , GeneralTheme "015" "éducation formation"
    , GeneralTheme "016" "recherche"
    , GeneralTheme "017" "santé"
    , GeneralTheme "018" "services et établissements médico-sociaux"
    , GeneralTheme "019" "interventions sociales"
    , GeneralTheme "020" "associations caritatives, humanitaires, aide au développement, développement du bénévolat"
    , GeneralTheme "021" "services familiaux, services aux personnes âgées"
    , GeneralTheme "022" "conduite d’activités économiques"
    , GeneralTheme "023" "représentation, promotion et défense d’intérêts économiques"
    , GeneralTheme "024" "environnement, cadre de vie"
    , GeneralTheme "030" "aide à l'emploi, développement local, promotion de solidarités économiques, vie locale"
    , GeneralTheme "032" "logement"
    , GeneralTheme "034" "tourisme"
    , GeneralTheme "036" "sécurité, protection civile"
    , GeneralTheme "038" "armée (dont préparation militaire, médailles)"
    , GeneralTheme "040" "activités religieuses, spirituelles ou philosophiques"
    , GeneralTheme "050" "domaines divers, domaines de nomenclature SITADELE à reclasser"
    ]

gtOptionList :: [(Text, Text)]
gtOptionList = toOption <$> generalThemes
    where toOption (GeneralTheme a b) = (b, a)