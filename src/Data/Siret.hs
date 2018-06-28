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

data Siret = Siret T.Text
           | UndefinedSiret

getSiren :: Siret -> T.Text
getSiren (Siret s) = T.take 9 s
getSiren _ = ""

getNic :: Siret -> T.Text
getNic (Siret s) = T.drop 9 s
getNic _ = ""

mkSiret :: T.Text -> Siret
mkSiret t | T.length t == 14 = Siret t
          | otherwise = UndefinedSiret

instance PersistField Siret where
    toPersistValue (Siret t) = PersistText t
    toPersistValue UndefinedSiret = PersistText ""

    fromPersistValue (PersistText t) = Right (mkSiret t)
    fromPersistValue _ = Left "Siret type works only with strings"

instance PersistFieldSql Siret where
    sqlType _ = SqlString

instance ToMarkup Siret where
    toMarkup UndefinedSiret = [shamlet|<span .text-muted>non renseigné|]
    toMarkup (Siret t) = string . T.unpack $ t

instance Empty Siret where
    isEmpty UndefinedSiret = True
    isEmpty _ = False