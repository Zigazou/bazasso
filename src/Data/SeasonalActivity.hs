{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.SeasonalActivity ( SeasonalActivity(..) ) where

import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, string, toMarkup)
import           Text.Hamlet            (shamlet)

import           Helpers.Empty          (Empty, isEmpty)

data SeasonalActivity = SAPermanent
                      | SASeasonal
                      | SANotSpecified
                      | UndefinedSeasonalActivity

instance PersistField SeasonalActivity where
    toPersistValue SAPermanent    = PersistText "P"
    toPersistValue SASeasonal     = PersistText "S"
    toPersistValue SANotSpecified = PersistText "NR"
    toPersistValue _              = PersistText ""

    fromPersistValue (PersistText "P") = Right SAPermanent
    fromPersistValue (PersistText "S") = Right SASeasonal
    fromPersistValue (PersistText "NR") = Right SANotSpecified
    fromPersistValue (PersistText _) = Right UndefinedSeasonalActivity
    fromPersistValue _ = Left "SeasonalActivity type works only with strings"

instance PersistFieldSql SeasonalActivity where
    sqlType _ = SqlString

instance ToMarkup SeasonalActivity where
    toMarkup SAPermanent    = string "permanente"
    toMarkup SASeasonal     = string "saisonnière"
    toMarkup SANotSpecified = string "non spécifiée"
    toMarkup _              = [shamlet|<span .text-muted>non renseigné|]

instance Empty SeasonalActivity where
    isEmpty UndefinedSeasonalActivity = True
    isEmpty _                         = False
