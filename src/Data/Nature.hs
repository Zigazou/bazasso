{- |
Module      :  Nature
Description :  Nature field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Nature field type
-}
module Data.Nature ( Nature, mkNature ) where

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

data Nature = Nature Char
            | UndefinedNature

mkNature :: T.Text -> Nature
mkNature t
    | T.length t == 1 && T.any (T.head t ==) "ABCDGILMRSU" = Nature (T.head t)
    | otherwise = UndefinedNature

instance PersistField Nature where
    toPersistValue (Nature n) = PersistText (T.singleton n)
    toPersistValue _          = PersistText ""

    fromPersistValue (PersistText t) = Right (mkNature t)
    fromPersistValue _ = Left "Nature type works only with strings"

instance PersistFieldSql Nature where
    sqlType _ = SqlString

instance ToMarkup Nature where
    toMarkup (Nature n) = string [n]
    toMarkup _          = [shamlet|<span .text-muted>non renseignée|]

instance Empty Nature where
    isEmpty UndefinedNature = True
    isEmpty _               = False
