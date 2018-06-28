{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Department
    ( getDepartmentCitiesR
    , getDepartmentListR
    )
    where

import Import

import Helpers.EntitiesToMaybe (entitiesToMaybe)

getDepartmentCitiesR :: Text -> Handler Html
getDepartmentCitiesR codedept = do
    cities <- runDB $ selectList [CommuneIddepartement ==. codedept]
                                 [Asc CommuneLibelle]

    mDept <- runDB $ entitiesToMaybe
                    <$> selectList [DepartementIdent ==. codedept] [LimitTo 1]

    defaultLayout $ do
        setTitle "Villes du départment"
        $(widgetFile "department-cities")

getDepartmentListR :: Handler Html
getDepartmentListR = do
    departments <- runDB (selectList [] [] :: DB [Entity Departement])

    defaultLayout $ do
        setTitle "Liste des départements"
        $(widgetFile "department-list")