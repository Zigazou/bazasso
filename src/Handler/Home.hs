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
    mNbNewAssos <- runDB $ singlesToMaybe <$> unsafeCount "rnawaldec"
    mNbOldAssos <- runDB $ singlesToMaybe <$> unsafeCount "rnaimport"
    mNbJOEntries <- runDB $ singlesToMaybe <$> unsafeCount "joannonce"

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")