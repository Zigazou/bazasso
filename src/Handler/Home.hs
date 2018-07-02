{- |
Module      :  Home
Description :  Handles home page requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles home page requests.
-}
module Handler.Home ( getHomeR ) where

import           Helpers.EntitiesToMaybe (singlesToMaybe)
import           Helpers.NumberFormat    (humanNumber)
import           Helpers.UnsafeCount     (unsafeCount)
import           Import

-- | Handles home page requests
getHomeR :: Handler Html
getHomeR = do
    -- Get numbers about the database
    mNbNewAssos  <- runDB $ singlesToMaybe <$> unsafeCount "rnawaldec"
    mNbOldAssos  <- runDB $ singlesToMaybe <$> unsafeCount "rnaimport"
    mNbJOEntries <- runDB $ singlesToMaybe <$> unsafeCount "joannonce"
    mNbSiren     <- runDB $ singlesToMaybe <$> unsafeCount "sirene"
    mNbCities    <- runDB $ singlesToMaybe <$> unsafeCount "commune"

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")
