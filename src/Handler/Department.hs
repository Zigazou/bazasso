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

getDepartmentCitiesR :: Text -> Handler Html
getDepartmentCitiesR codedept = do
    let query :: DB [Entity Commune]
        query = selectList [CommuneIddepartement ==. codedept]
                           [Asc CommuneLibelle]

    cities <- runDB query

    defaultLayout $ do
        setTitle "Villes du départment"
        $(widgetFile "department-cities")

getDepartmentListR :: Handler Html
getDepartmentListR = do
    departments <- runDB (selectList [] [] :: DB [Entity Departement])

    defaultLayout $ do
        setTitle "Liste des départements"
        $(widgetFile "department-list")