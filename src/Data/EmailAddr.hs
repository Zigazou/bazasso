{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.EmailAddr
    ( EmailAddr
    , mkEmailAddr
    ) where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql
    (PersistFieldSql, sqlType, PersistValue(PersistText), SqlType(SqlString))
import Text.Blaze (ToMarkup, toMarkup)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Hamlet (hamlet, shamlet)
import Text.Email.Parser (EmailAddress)
import Text.Email.Validate (emailAddress, toByteString)

import Helpers.Empty (Empty, isEmpty)

data EmailAddr = EmailAddr EmailAddress
               | UndefinedEmailAddress

mkEmailAddr :: T.Text -> EmailAddr
mkEmailAddr t = case emailAddress (encodeUtf8 t) of
                    Nothing -> UndefinedEmailAddress
                    Just addr -> EmailAddr addr

toText :: EmailAddress -> T.Text
toText = decodeUtf8 . toByteString

instance PersistField EmailAddr where
    toPersistValue (EmailAddr t) = PersistText . toText $ t
    toPersistValue _ = PersistText ""

    fromPersistValue (PersistText t) = Right (mkEmailAddr t)
    fromPersistValue _ = Left "EmailAddr type works only with strings"

instance PersistFieldSql EmailAddr where
    sqlType _ = SqlString

instance ToMarkup EmailAddr where
    toMarkup UndefinedEmailAddress = [shamlet|<span .text-muted>non renseignée|]
    toMarkup (EmailAddr t) = [hamlet|<a href=@{toText t}>#{toText t}|] renderMailTo
        where
            renderMailTo u _ = T.concat [ "mailto:", u ]

instance Empty EmailAddr where
    isEmpty UndefinedEmailAddress = True
    isEmpty _ = False