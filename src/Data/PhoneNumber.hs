{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.PhoneNumber
    ( PhoneNumber
    , mkPhoneNumber
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import qualified Data.Text as T
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

data PhoneNumber = PhoneNumber T.Text
                 | UndefinedPhoneNumber

mkPhoneNumber :: T.Text -> PhoneNumber
mkPhoneNumber t | T.length t == 10 && T.head t == '0' = PhoneNumber t
                | otherwise = UndefinedPhoneNumber

instance PersistField PhoneNumber where
    toPersistValue (PhoneNumber t) = PersistText t
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText t) = Right (mkPhoneNumber t)
    fromPersistValue _ = Left "PhoneNumber type works only with strings"

instance PersistFieldSql PhoneNumber where
    sqlType _ = SqlString

instance ToMarkup PhoneNumber where
    toMarkup UndefinedPhoneNumber = [shamlet|<span .text-muted>non renseigné|]
    toMarkup (PhoneNumber t) = string
                             . T.unpack
                             . T.intercalate " "
                             . T.chunksOf 2
                             $ t

instance Empty PhoneNumber where
    isEmpty UndefinedPhoneNumber = True
    isEmpty _ = False