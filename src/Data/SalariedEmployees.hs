{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SalariedEmployees
    ( SalariedEmployees(..)
    , mkSalariedEmployees
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup, string)
import Text.Hamlet (shamlet)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (fromMaybe)

import Helpers.Empty (Empty, isEmpty)

data SalariedEmployees = SalariedEmployees T.Text
                       | UndefinedSalariedEmployees

validCodes :: [(T.Text, T.Text)]
validCodes =
    [ ("NN", "Unité non employeuse ou sans mise à jour d'effectif")
    , ("00", "0 salarié·e")
    , ("01", "1 ou 2 salarié·e·s")
    , ("02", "3 à 5 salarié·e·s")
    , ("03", "6 à 9 salarié·e·s")
    , ("11", "10 à 19 salarié·e·s")
    , ("12", "20 à 49 salarié·e·s")
    , ("21", "50 à 99 salarié·e·s")
    , ("22", "100 à 199 salarié·e·s")
    , ("31", "200 à 249 salarié·e·s")
    , ("32", "250 à 499 salarié·e·s")
    , ("41", "500 à 999 salarié·e·s")
    , ("42", "1000 à 1999 salarié·e·s")
    , ("51", "2000 à 4999 salarié·e·s")
    , ("52", "5000 à 9999 salarié·e·s")
    , ("53", "10000 salarié·e·s et plus")
    ]

getTitle :: T.Text -> T.Text
getTitle se = snd $ fromMaybe ("", "") $ find ((se ==) . fst) validCodes

mkSalariedEmployees :: T.Text -> SalariedEmployees
mkSalariedEmployees t
    | T.null (getTitle t) = UndefinedSalariedEmployees
    | otherwise = SalariedEmployees t

instance PersistField SalariedEmployees where
    toPersistValue (SalariedEmployees se) = PersistText se
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText t) = Right (mkSalariedEmployees t)
    fromPersistValue _ = Left "SalariedEmployees type works only with strings"

instance PersistFieldSql SalariedEmployees where
    sqlType _ = SqlString

instance ToMarkup SalariedEmployees where
    toMarkup (SalariedEmployees se) = string . T.unpack . getTitle $ se
    toMarkup _ = [shamlet|<span .text-muted>non renseignée|]

instance Empty SalariedEmployees where
    isEmpty UndefinedSalariedEmployees = True
    isEmpty _ = False