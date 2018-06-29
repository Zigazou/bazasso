{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Ess ( Ess(..) ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)

import Helpers.Empty (Empty, isEmpty)

data Ess = Ess
         | NotEss
         | EssInvalidated
         | UndefinedEss

instance PersistField Ess where
    toPersistValue Ess = PersistText "O"
    toPersistValue NotEss = PersistText "N"
    toPersistValue EssInvalidated = PersistText "I"
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText "O") = Right Ess
    fromPersistValue (PersistText "N") = Right NotEss
    fromPersistValue (PersistText "I") = Right EssInvalidated
    fromPersistValue (PersistText _) = Right UndefinedEss
    fromPersistValue _ = Left "Ess type works only with strings"

instance PersistFieldSql Ess where
    sqlType _ = SqlString

instance ToMarkup Ess where
    toMarkup Ess = string "l'entreprise appartient au champ de l'économie \
                          \sociale et solidaire"
    toMarkup NotEss = string "l'entreprise n'appartient pas au champ de \
                             \l'économie sociale et solidaire"
    toMarkup EssInvalidated = string "appartenance invalidée par les greffes"
    toMarkup _ = string "l'entreprise n'est pas concernée par l'économie \
                        \sociale et solidaire"

instance Empty Ess where
    isEmpty UndefinedEss = True
    isEmpty _ = False