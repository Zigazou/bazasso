{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home
    ( getHomeR
    ) where

import           Helpers.EntitiesToMaybe (singlesToMaybe)
import           Helpers.NumberFormat    (humanNumber)
import           Helpers.UnsafeCount     (unsafeCount)
import           Import

getHomeR :: Handler Html
getHomeR = do
    mNbNewAssos  <- runDB $ singlesToMaybe <$> unsafeCount "rnawaldec"
    mNbOldAssos  <- runDB $ singlesToMaybe <$> unsafeCount "rnaimport"
    mNbJOEntries <- runDB $ singlesToMaybe <$> unsafeCount "joannonce"
    mNbSiren     <- runDB $ singlesToMaybe <$> unsafeCount "sirene"
    mNbCities    <- runDB $ singlesToMaybe <$> unsafeCount "commune"

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")
