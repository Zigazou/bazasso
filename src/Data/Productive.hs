{- |
Module      :  Productive
Description :  Productive field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Productive field type
-}
module Data.Productive ( Productive(..) ) where

import           ClassyPrelude.Yesod
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, string, toMarkup)
import           Text.Hamlet            (shamlet)

import           Helpers.Empty          (Empty, isEmpty)

data Productive = Productive
                | NotProductive
                | UndefinedProductive

instance PersistField Productive where
    toPersistValue Productive    = PersistText "O"
    toPersistValue NotProductive = PersistText "N"
    toPersistValue _             = PersistText ""

    fromPersistValue (PersistText "O") = Right Productive
    fromPersistValue (PersistText "N") = Right NotProductive
    fromPersistValue (PersistText _) = Right UndefinedProductive
    fromPersistValue _ = Left "Productive type works only with strings"

instance PersistFieldSql Productive where
    sqlType _ = SqlString

instance ToMarkup Productive where
    toMarkup Productive    = string "productif"
    toMarkup NotProductive = string "non productif"
    toMarkup _             = [shamlet|<span .text-muted>non renseigné|]

instance Empty Productive where
    isEmpty UndefinedProductive = True
    isEmpty _                   = False
