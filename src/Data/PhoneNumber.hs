{- |
Module      :  PhoneNumber
Description :  Phone number field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Phone number field type
-}
module Data.PhoneNumber
    ( PhoneNumber
    , mkPhoneNumber
    ) where

import           ClassyPrelude.Yesod
import qualified Data.Text              as T
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, toMarkup)
import           Text.Hamlet            (hamlet, shamlet)

import           Helpers.Empty          (Empty, isEmpty)

data PhoneNumber = PhoneNumber T.Text
                 | UndefinedPhoneNumber

mkPhoneNumber :: T.Text -> PhoneNumber
mkPhoneNumber t | T.length t == 10 && T.head t == '0' = PhoneNumber t
                | otherwise = UndefinedPhoneNumber

instance PersistField PhoneNumber where
    toPersistValue (PhoneNumber t) = PersistText t
    toPersistValue _               = PersistText ""

    fromPersistValue (PersistText t) = Right (mkPhoneNumber t)
    fromPersistValue _ = Left "PhoneNumber type works only with strings"

instance PersistFieldSql PhoneNumber where
    sqlType _ = SqlString

instance ToMarkup PhoneNumber where
    toMarkup UndefinedPhoneNumber = [shamlet|<span .text-muted>non renseigné|]
    toMarkup (PhoneNumber t) = [hamlet|<a href=@{t}>#{human}|] renderTel
        where
            human = T.intercalate " " . T.chunksOf 2 $ t
            renderTel u _ = T.concat [ "tel:+33", T.tail u ]

instance Empty PhoneNumber where
    isEmpty UndefinedPhoneNumber = True
    isEmpty _                    = False
