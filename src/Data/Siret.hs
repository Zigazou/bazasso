{- |
Module      :  Siret
Description :  Siret number field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Siret number field type
-}
module Data.Siret
    ( Siret
    , getSiren
    , getNic
    , mkSiret
    ) where

import           ClassyPrelude.Yesod
import qualified Data.Text              as T
import           Database.Persist.Class (PersistField, fromPersistValue,
                                         toPersistValue)
import           Database.Persist.Sql   (PersistFieldSql,
                                         PersistValue (PersistText),
                                         SqlType (SqlString), sqlType)
import           Text.Blaze             (ToMarkup, string, toMarkup)
import           Text.Hamlet            (shamlet)

import           Helpers.Empty          (Empty, isEmpty)

data Siret = Siret T.Text
           | UndefinedSiret

getSiren :: Siret -> T.Text
getSiren (Siret s) = T.take 9 s
getSiren _         = ""

getNic :: Siret -> T.Text
getNic (Siret s) = T.drop 9 s
getNic _         = ""

mkSiret :: T.Text -> Siret
mkSiret t | T.length t == 14 = Siret t
          | otherwise = UndefinedSiret

instance PersistField Siret where
    toPersistValue (Siret t)      = PersistText t
    toPersistValue UndefinedSiret = PersistText ""

    fromPersistValue (PersistText t) = Right (mkSiret t)
    fromPersistValue _ = Left "Siret type works only with strings"

instance PersistFieldSql Siret where
    sqlType _ = SqlString

instance ToMarkup Siret where
    toMarkup UndefinedSiret = [shamlet|<span .text-muted>non renseigné|]
    toMarkup (Siret t)      = string . T.unpack $ t

instance Empty Siret where
    isEmpty UndefinedSiret = True
    isEmpty _              = False
