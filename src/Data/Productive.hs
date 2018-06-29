{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Productive ( Productive(..) ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

data Productive = Productive
                | NotProductive
                | UndefinedProductive

instance PersistField Productive where
    toPersistValue Productive = PersistText "O"
    toPersistValue NotProductive = PersistText "N"
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText "O") = Right Productive
    fromPersistValue (PersistText "N") = Right NotProductive
    fromPersistValue (PersistText _) = Right UndefinedProductive
    fromPersistValue _ = Left "Productive type works only with strings"

instance PersistFieldSql Productive where
    sqlType _ = SqlString

instance ToMarkup Productive where
    toMarkup Productive = string "productif"
    toMarkup NotProductive = string "non productif"
    toMarkup _ = [shamlet|<span .text-muted>non renseigné|]

instance Empty Productive where
    isEmpty UndefinedProductive = True
    isEmpty _ = False