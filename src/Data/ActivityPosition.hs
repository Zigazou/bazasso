{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.ActivityPosition ( ActivityPosition(..) ) where

import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, string, toMarkup)
import           Text.Hamlet            (shamlet)

import           Helpers.Empty          (Empty, isEmpty)

data ActivityPosition = ActiveAssociation
                      | DissolvedAssociation
                      | DeletedAssociation
                      | UndefinedActivity

instance PersistField ActivityPosition where
    toPersistValue ActiveAssociation    = PersistText "A"
    toPersistValue DissolvedAssociation = PersistText "D"
    toPersistValue DeletedAssociation   = PersistText "S"
    toPersistValue _                    = PersistText ""

    fromPersistValue (PersistText "A") = Right ActiveAssociation
    fromPersistValue (PersistText "D") = Right DissolvedAssociation
    fromPersistValue (PersistText "S") = Right DeletedAssociation
    fromPersistValue (PersistText _) = Right UndefinedActivity
    fromPersistValue _ = Left "ActivityPosition type works only with strings"

instance PersistFieldSql ActivityPosition where
    sqlType _ = SqlString

instance ToMarkup ActivityPosition where
    toMarkup ActiveAssociation = string "active"
    toMarkup DissolvedAssociation = string "dissoute"
    toMarkup DeletedAssociation = string "supprimée"
    toMarkup _ = [shamlet|<span .text-muted>non renseignée|]

instance Empty ActivityPosition where
    isEmpty UndefinedActivity = True
    isEmpty _                 = False
