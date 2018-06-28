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
import Helpers.Empty (Empty(..))
import Helpers.Clean (clean)
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (match)
import Helpers.EntitiesToMaybe (entitiesToMaybe)

import Widgets.ActivityForm (activityForm, ActivityForm(..))

import Data.List (foldl)
import qualified Data.Text as T

lookForNewAssociations :: DBparam ActivityForm [Entity Rnawaldec]
lookForNewAssociations (ActivityForm insee themes) = selectList
    ([RnawaldecAdrscodeinsee `match` insee] ++ themesFilter themes)
    [LimitTo 1000]
    where
        addTheme [] b = [ RnawaldecObjetsocial1 >=. T.concat [b, "000"]
                        , RnawaldecObjetsocial1 <=. T.concat [b, "999"]
                        ]
                    ||. [ RnawaldecObjetsocial2 >=. T.concat [b, "000"]
                        , RnawaldecObjetsocial2 <=. T.concat [b, "999"]
                        ]
        addTheme a b = a ||. addTheme [] b
        themesFilter = foldl addTheme []

lookForOldAssociations :: DBparam ActivityForm [Entity Rnaimport]
lookForOldAssociations (ActivityForm insee themes) = selectList
    ([RnaimportAdrscodeinsee `match` insee] ++ themesFilter themes)
    [LimitTo 1000]
    where
        addTheme [] b = [ RnaimportObjetsocial1 >=. T.concat [b, "000"]
                        , RnaimportObjetsocial1 <=. T.concat [b, "999"]
                        ]
                    ||. [ RnaimportObjetsocial2 >=. T.concat [b, "000"]
                        , RnaimportObjetsocial2 <=. T.concat [b, "999"]
                        ]
        addTheme a b = a ||. addTheme [] b
        themesFilter = foldl addTheme []

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
