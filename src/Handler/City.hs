{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  City
Description :  Handles city page requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles city page requests.
-}
module Handler.City
    ( getCityActivityR
    , postCityActivityR
    ) where

import           Helpers.EntitiesToMaybe (entitiesToMaybe)
import           Helpers.GeneralTheme    (themesFilterNew, themesFilterOld)
import           Helpers.Like            (match)
import           Import

import           Widgets.ActivityForm    (ActivityForm (..), activityForm)
import           Widgets.SearchResults   (searchResults)

-- | Query to retrieve new associations based on an activity form
lookForNewAssociations :: DBparam ActivityForm [Entity Rnawaldec]
lookForNewAssociations (ActivityForm insee themes) = selectList
    (RnawaldecAdrscodeinsee `match` insee : themesFilterNew themes)
    [LimitTo 1000]

-- | Query to retrieve old associations based on an activity form
lookForOldAssociations :: DBparam ActivityForm [Entity Rnaimport]
lookForOldAssociations (ActivityForm insee themes) = selectList
    (RnaimportAdrscodeinsee `match` insee : themesFilterOld themes)
    [LimitTo 1000]

-- | Handles GET requests about a city
getCityActivityR :: Text         -- ^ Insee code identifying a city
                 -> Handler Html -- ^ Request handler
getCityActivityR insee = do
    (formWidget, formEnctype) <- generateFormPost (activityForm insee)

    mCity <- runDB $ entitiesToMaybe <$> selectList [CommuneCodeinsee ==. insee]
                                                    [LimitTo 1]

    let (newassos, oldassos) = ([], [])

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")

-- | Handles POST requests about a city
postCityActivityR :: Text         -- ^ Insee code identifying a city
                  -> Handler Html -- ^ Request handler
postCityActivityR insee = do
    ((formResult, formWidget), formEnctype) <- runFormPost (activityForm insee)

    mCity <- runDB $ entitiesToMaybe <$> selectList [CommuneCodeinsee ==. insee]
                                                    [LimitTo 1]

    newassos <- case formResult of
        FormSuccess search -> runDB $ lookForNewAssociations search
        _                  -> return []

    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        _                  -> return []

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")
