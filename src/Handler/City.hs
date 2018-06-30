{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.City
    ( getCityActivityR
    , postCityActivityR
    ) where

import Import
import Helpers.Like (match)
import Helpers.EntitiesToMaybe (entitiesToMaybe)
import Helpers.GeneralTheme (themesFilterNew, themesFilterOld)

import Widgets.ActivityForm (activityForm, ActivityForm(..))
import Widgets.SearchResults (searchResults)

lookForNewAssociations :: DBparam ActivityForm [Entity Rnawaldec]
lookForNewAssociations (ActivityForm insee themes) = selectList
    (RnawaldecAdrscodeinsee `match` insee : themesFilterNew themes)
    [LimitTo 1000]

lookForOldAssociations :: DBparam ActivityForm [Entity Rnaimport]
lookForOldAssociations (ActivityForm insee themes) = selectList
    (RnaimportAdrscodeinsee `match` insee : themesFilterOld themes)
    [LimitTo 1000]

getCityActivityR :: Text -> Handler Html
getCityActivityR insee = do
    (formWidget, formEnctype) <- generateFormPost (activityForm insee)

    mCity <- runDB $ entitiesToMaybe <$> selectList [CommuneCodeinsee ==. insee]
                                                    [LimitTo 1]

    let (newassos, oldassos) = ([], [])

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")

postCityActivityR :: Text -> Handler Html
postCityActivityR insee = do
    ((formResult, formWidget), formEnctype) <- runFormPost (activityForm insee)

    mCity <- runDB $ entitiesToMaybe <$> selectList [CommuneCodeinsee ==. insee]
                                                    [LimitTo 1]

    newassos <- case formResult of
        FormSuccess search -> runDB $ lookForNewAssociations search
        FormMissing -> return []
        FormFailure _ -> return []

    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        FormMissing -> return []
        FormFailure _ -> return []

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")
