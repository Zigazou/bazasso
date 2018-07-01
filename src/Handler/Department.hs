{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Department
    ( getDepartmentCitiesR
    , getDepartmentListR
    )
    where

import           Import

getDepartmentCitiesR :: DepartementId -> Handler Html
getDepartmentCitiesR codedept = do
    cities <- runDB $ selectList [CommuneIddepartement ==. codedept]
                                 [Asc CommuneLibelle]

    mDept <- runDB . get $ codedept

    defaultLayout $ do
        setTitle "Villes du départment"
        $(widgetFile "department-cities")

getDepartmentListR :: Handler Html
getDepartmentListR = do
    departments <- runDB (selectList [] [] :: DB [Entity Departement])

    defaultLayout $ do
        setTitle "Liste des départements"
        $(widgetFile "department-list")
