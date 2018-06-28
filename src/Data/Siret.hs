{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Siret
    ( Siret
    , getSiren
    , getNic
    , mkSiret
    ) where

import Prelude
import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import qualified Data.Text as T

import Helpers.Empty (Empty, isEmpty)

data Siret = Siret T.Text

getSiren :: Siret -> T.Text
getSiren (Siret s) = T.take 9 s

getNic :: Siret -> T.Text
getNic (Siret s) = T.drop 9 s

mkSiret :: T.Text -> Siret
mkSiret = Siret

instance PersistField Siret where
    toPersistValue (Siret t) = PersistText t

    fromPersistValue (PersistText t) =
        case T.length t of
            14 -> Right (Siret t)
            0 -> Right (Siret "")
            _ -> Left "Siret must contain exactly 14 characters"
    fromPersistValue _ = Left "Siret type works only with strings"

instance PersistFieldSql Siret where
    sqlType _ = SqlString

instance ToMarkup Siret where
    toMarkup (Siret t) = string . T.unpack $ t

instance Empty Siret where
    isEmpty (Siret t) = T.length t == 0