{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home
    ( getHomeR
    ) where

import Import
import Helpers.UnsafeCount (unsafeCount)
import Helpers.EntitiesToMaybe (singlesToMaybe)

getHomeR :: Handler Html
getHomeR = do
    mNbNewAssos <- runDB $ unsafeCount "rnawaldec" >>= return . singlesToMaybe
    mNbOldAssos <- runDB $ unsafeCount "rnaimport" >>= return . singlesToMaybe
    mNbJOEntries <- runDB $ unsafeCount "joannonce" >>= return . singlesToMaybe

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")