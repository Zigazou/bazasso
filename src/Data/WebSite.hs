{- |
Module      :  WebSite
Description :  Web site field type
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Web site field type
-}
module Data.WebSite
    ( WebSite
    , mkWebSite
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

data WebSite = WebSite T.Text
             | UndefinedWebSite

mkWebSite :: T.Text -> WebSite
mkWebSite t = case T.breakOnEnd "//" t of
                (_, "")   -> UndefinedWebSite
                ("", url) -> WebSite (T.concat ["http://", url])
                (_, url)  -> WebSite url

instance PersistField WebSite where
    toPersistValue (WebSite t) = PersistText t
    toPersistValue _           = PersistText ""

    fromPersistValue (PersistText t) = Right (mkWebSite t)
    fromPersistValue _ = Left "WebSite type works only with strings"

instance PersistFieldSql WebSite where
    sqlType _ = SqlString

instance ToMarkup WebSite where
    toMarkup UndefinedWebSite = [shamlet|<span .text-muted>non renseigné|]
    toMarkup (WebSite t) = [hamlet|<a href=@{t}>#{t}|] renderUrl
        where renderUrl u _ = u

instance Empty WebSite where
    isEmpty UndefinedWebSite = True
    isEmpty _                = False
