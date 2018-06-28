{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.WebSite
    ( WebSite
    , mkWebSite
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup)
import qualified Data.Text as T
import Text.Hamlet (hamlet, shamlet)

import Helpers.Empty (Empty, isEmpty)

data WebSite = WebSite T.Text
             | UndefinedWebSite

mkWebSite :: T.Text -> WebSite
mkWebSite t = case T.breakOnEnd "//" t of
                (_, "") -> UndefinedWebSite
                ("", url) -> WebSite (T.concat ["http://", url])
                (_, url) -> WebSite url

instance PersistField WebSite where
    toPersistValue (WebSite t) = PersistText t
    toPersistValue _ = PersistText ""

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
    isEmpty _ = False