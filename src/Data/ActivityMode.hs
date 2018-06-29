{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.ActivityMode ( ActivityMode(..) ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

data ActivityMode = AMOrderingParty
                  | AMAssembly
                  | AMRepair
                  | AMSimple
                  | UndefinedActivityMode

instance PersistField ActivityMode where
    toPersistValue AMOrderingParty = PersistText "D"
    toPersistValue AMAssembly = PersistText "M"
    toPersistValue AMRepair = PersistText "R"
    toPersistValue AMSimple = PersistText "S"
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText "D") = Right AMOrderingParty
    fromPersistValue (PersistText "M") = Right AMAssembly
    fromPersistValue (PersistText "R") = Right AMRepair
    fromPersistValue (PersistText "S") = Right AMSimple
    fromPersistValue (PersistText _) = Right UndefinedActivityMode
    fromPersistValue _ = Left "ActivityMode type works only with strings"

instance PersistFieldSql ActivityMode where
    sqlType _ = SqlString

instance ToMarkup ActivityMode where
    toMarkup AMOrderingParty = string "donneur d'ordre"
    toMarkup AMAssembly = string "montage, installation"
    toMarkup AMRepair = string "réparation"
    toMarkup AMSimple = string "activité simple"
    toMarkup _ = [shamlet|<span .text-muted>non renseigné|]

instance Empty ActivityMode where
    isEmpty UndefinedActivityMode = True
    isEmpty _ = False