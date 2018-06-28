{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Rup
    ( Rup
    , mkRup
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import qualified Data.Text as T
import Text.Hamlet (shamlet)

import Helpers.Empty (Empty, isEmpty)

newtype Rup = Rup T.Text

mkRup :: T.Text -> Rup
mkRup t | T.length t == 14 && length (T.splitOn "." t) == 3 = Rup t
        | otherwise = Rup ""

instance PersistField Rup where
    toPersistValue (Rup t) = PersistText t

    fromPersistValue (PersistText t) = Right (mkRup t)
    fromPersistValue _ = Left "Rup type works only with strings"

instance PersistFieldSql Rup where
    sqlType _ = SqlString

instance ToMarkup Rup where
    toMarkup (Rup t)
        | T.length t == 0 = [shamlet|<span .text-muted>non renseigné|]
        | otherwise = string . T.unpack $ t

instance Empty Rup where
    isEmpty (Rup t) = T.length t == 0