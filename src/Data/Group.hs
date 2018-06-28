{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Group ( Group(..) ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

data Group = GroupSimple
           | GroupUnion
           | GroupFederation
           | UndefinedGroup

instance PersistField Group where
    toPersistValue GroupSimple = PersistText "S"
    toPersistValue GroupUnion = PersistText "U"
    toPersistValue GroupFederation = PersistText "F"
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText "S") = Right GroupSimple
    fromPersistValue (PersistText "U") = Right GroupUnion
    fromPersistValue (PersistText "F") = Right GroupFederation
    fromPersistValue (PersistText _) = Right UndefinedGroup
    fromPersistValue _ = Left "Group type works only with strings"

instance PersistFieldSql Group where
    sqlType _ = SqlString

instance ToMarkup Group where
    toMarkup GroupSimple = string "simple"
    toMarkup GroupUnion = string "union"
    toMarkup GroupFederation = string "fédération"
    toMarkup _ = [shamlet|<span .text-muted>non renseigné|]

instance Empty Group where
    isEmpty UndefinedGroup = True
    isEmpty _ = False