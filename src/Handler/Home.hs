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
import Helpers.NumberFormat (humanNumber)

getHomeR :: Handler Html
getHomeR = do
    mNbNewAssos <- runDB $ singlesToMaybe <$> unsafeCount "rnawaldec"
    mNbOldAssos <- runDB $ singlesToMaybe <$> unsafeCount "rnaimport"
    mNbJOEntries <- runDB $ singlesToMaybe <$> unsafeCount "joannonce"
    mNbSiren <- runDB $ singlesToMaybe <$> unsafeCount "sirene"
    mNbCities <- runDB $ singlesToMaybe <$> unsafeCount "commune"

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")