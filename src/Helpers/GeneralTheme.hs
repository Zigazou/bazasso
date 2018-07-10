{- |
Module      :  GeneralTheme
Description :  General themes from the Journal Officiel
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Themes from the Journal Officiel consist of 6 digits. The first 3 digits
indicate a general theme. This module allows to search themes based on the
first 3 digits.
-}
module Helpers.GeneralTheme
    ( generateKeys
    , themesFilterNew
    , themesFilterOld
    , GeneralTheme(..)
    , generalThemes
    , gtOptionList
    ) where

import           Import

import           Data.Either (either)
import           Data.List   (foldl)
import qualified Data.Text   as T

-- | A general theme is composed of the first 3 digits and a name describing
--   the general theme.
data GeneralTheme = AllGeneralTheme
                  | GeneralTheme
                        { gtStart :: Text -- ^ The first 3 digits of the theme
                        , gtName  :: Text -- ^ The name of the general theme
                        }
                  deriving (Eq, Show)

gtFirst :: GeneralTheme -> Text
gtFirst gt = T.concat [ gtStart gt, "000" ]

gtLast :: GeneralTheme -> Text
gtLast gt = T.concat [ gtLast gt, "999" ]

-- | Generates a pair of (starting, ending) keys which allows to search for
--   specific themes pertaining to a general theme.
generateKeys :: GeneralTheme -- ^ The general theme (3 digits)
             -> Maybe (Key Jotheme, Key Jotheme) -- ^ The resulting pair
generateKeys AllGeneralTheme = either (const Nothing) Just eKeys
    where
        eKeys :: Either T.Text (Key Jotheme, Key Jotheme)
        eKeys = do
            fstKey <- keyFromValues [PersistText "000000"]
            sndKey <- keyFromValues [PersistText "999999"]
            return (fstKey, sndKey)

generateKeys theme = either (const Nothing) Just eKeys
    where
        eKeys :: Either T.Text (Key Jotheme, Key Jotheme)
        eKeys = do
            fstKey <- keyFromValues [PersistText (gtFirst theme)]
            sndKey <- keyFromValues [PersistText (gtLast theme)]
            return (fstKey, sndKey)

-- | Adds filters to an existing list of filters
addTheme :: EntityField db (Key Jotheme) -- ^ First field
         -> EntityField db (Key Jotheme) -- ^ Second field
         -> [Filter db]                  -- ^ List of filters to populate
         -> (Key Jotheme, Key Jotheme)   -- ^ Starting and ending keys
         -> [Filter db]                  -- ^ Resulting list of filters
addTheme fstField sndField [] (fstKey, sndKey) =
        [ fstField >=. fstKey, fstField <=. sndKey ]
    ||. [ sndField >=. fstKey, sndField <=. sndKey ]
addTheme fstField sndField a b = a ||. addTheme fstField sndField [] b

-- | Generates a list of `or` filters searching for themes in the new database.
--   This list can then be used with the `selectList` function
themesFilterNew :: [GeneralTheme]     -- ^ List of general themes
                -> [Filter Rnawaldec] -- ^ Resulting list of filters
themesFilterNew themes =
    foldl (addTheme RnawaldecObjetsocial1 RnawaldecObjetsocial2)
          []
          (catMaybes (generateKeys <$> themes))

-- | Generates a list of `or` filters searching for themes in the old database.
--   This list can then be used with the `selectList` function
themesFilterOld :: [GeneralTheme]     -- ^ List of general themes
                -> [Filter Rnaimport] -- ^ Resulting list of filters
themesFilterOld themes =
    foldl (addTheme RnaimportObjetsocial1 RnaimportObjetsocial2)
          []
          (catMaybes (generateKeys <$> themes))

-- | List of a known general themes.
generalThemes :: [GeneralTheme]
generalThemes =
    [ AllGeneralTheme
    , GeneralTheme "001" "activités politiques"
    , GeneralTheme "002" "clubs, cercles de réflexion"
    , GeneralTheme "003" "défense de droits fondamentaux, activités civiques"
    , GeneralTheme "004" "justice"
    , GeneralTheme "005" "information communication"
    , GeneralTheme "006" "culture, pratiques d’activités artistiques, \
                         \culturelles"
    , GeneralTheme "007" "clubs de loisirs, relations"
    , GeneralTheme "009" "action socio-culturelle"
    , GeneralTheme "010" "préservation du patrimoine"
    , GeneralTheme "011" "sports, activités de plein air"
    , GeneralTheme "013" "chasse pêche"
    , GeneralTheme "014" "amicales, groupements affinitaires, groupements \
                         \d'entraide (hors défense de droits fondamentaux"
    , GeneralTheme "015" "éducation formation"
    , GeneralTheme "016" "recherche"
    , GeneralTheme "017" "santé"
    , GeneralTheme "018" "services et établissements médico-sociaux"
    , GeneralTheme "019" "interventions sociales"
    , GeneralTheme "020" "associations caritatives, humanitaires, aide au \
                         \développement, développement du bénévolat"
    , GeneralTheme "021" "services familiaux, services aux personnes âgées"
    , GeneralTheme "022" "conduite d’activités économiques"
    , GeneralTheme "023" "représentation, promotion et défense d’intérêts \
                         \économiques"
    , GeneralTheme "024" "environnement, cadre de vie"
    , GeneralTheme "030" "aide à l'emploi, développement local, promotion de \
                         \solidarités économiques, vie locale"
    , GeneralTheme "032" "logement"
    , GeneralTheme "034" "tourisme"
    , GeneralTheme "036" "sécurité, protection civile"
    , GeneralTheme "038" "armée (dont préparation militaire, médailles)"
    , GeneralTheme "040" "activités religieuses, spirituelles ou philosophiques"
    , GeneralTheme "050" "domaines divers, domaines de nomenclature SITADELE à \
                         \reclasser"
    ]

-- | Helper function giving the list of general themes in a form suitable for
--   use with a SelectField.
gtOptionList :: [(Text, GeneralTheme)]
gtOptionList = toOption <$> generalThemes
    where toOption theme@(GeneralTheme _ b) = (b, theme)
          toOption theme                    = ("Dans tous les thèmes", theme)
