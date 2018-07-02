{- |
Module      :  Ess
Description :  Economie Sociale et Solidaire field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Economie Sociale et Solidaire field type
-}
module Data.Ess ( Ess(..) ) where

import           ClassyPrelude.Yesod
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, string, toMarkup)

import           Helpers.Empty          (Empty, isEmpty)

data Ess = Ess
         | NotEss
         | EssInvalidated
         | UndefinedEss

instance PersistField Ess where
    toPersistValue Ess            = PersistText "O"
    toPersistValue NotEss         = PersistText "N"
    toPersistValue EssInvalidated = PersistText "I"
    toPersistValue _              = PersistText ""

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
    isEmpty _            = False
