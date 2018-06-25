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
import Helpers.Empty (clean, Empty(..))
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (startsLike)

import Widgets.ActivityForm (activityForm, ActivityForm(..))

import Data.List (foldl)

lookForAssociations :: DBparam ActivityForm [Entity Rnawaldec]
lookForAssociations (ActivityForm insee themes) = selectList
    ([RnawaldecAdrscodeinsee ==. insee] ++ themesFilter themes)
    [LimitTo 1000]
    where
        addTheme [] b = [RnawaldecObjetsocial1 `startsLike` b]
                    ||. [RnawaldecObjetsocial2 `startsLike` b]
        addTheme a b = a ||. addTheme [] b
        themesFilter ts = foldl addTheme [] ts

getCityActivityR :: Text -> Handler Html
getCityActivityR insee = do
    (formWidget, formEnctype) <- generateFormPost (activityForm insee)

    let associations = []

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")

postCityActivityR :: Text -> Handler Html
postCityActivityR insee = do
    ((formResult, formWidget), formEnctype) <- runFormPost (activityForm insee)

    associations <- case formResult of
        FormSuccess search -> runDB $ lookForAssociations search
        FormMissing -> return []
        FormFailure _ -> return []

    defaultLayout $ do
        setTitle "Domaines d'activités"
        $(widgetFile "city-activity")
