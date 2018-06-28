{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Siret
    ( Siret
    , getSiren
    , getNic
    , mkSiret
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import qualified Data.Text as T
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

newtype Siret = Siret T.Text

getSiren :: Siret -> T.Text
getSiren (Siret s) = T.take 9 s

getNic :: Siret -> T.Text
getNic (Siret s) = T.drop 9 s

mkSiret :: T.Text -> Siret
mkSiret t | T.length t == 14 = Siret t
          | otherwise = Siret ""

instance PersistField Siret where
    toPersistValue (Siret t) = PersistText t

    fromPersistValue (PersistText t) = Right (mkSiret t)
    fromPersistValue _ = Left "Siret type works only with strings"

instance PersistFieldSql Siret where
    sqlType _ = SqlString

instance ToMarkup Siret where
    toMarkup (Siret t) | T.null t = [shamlet|<span .text-muted>non renseigné|]
                       | otherwise = string . T.unpack $ t

instance Empty Siret where
    isEmpty (Siret t) = T.null t