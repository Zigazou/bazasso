{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  Department
Description :  Handles department requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles department requests.
-}
module Handler.Department
    ( getDepartmentCitiesR
    , getDepartmentListR
    )
    where

import           Import

-- | Handles GET requests giving the list of cities in a departement
getDepartmentCitiesR :: DepartementId -- ^ Department identifier
                     -> Handler Html  -- ^ Resulting handler
getDepartmentCitiesR codedept = do
    cities <- runDB $ selectList [CommuneIddepartement ==. codedept]
                                 [Asc CommuneLibelle]

    mDept <- runDB . get $ codedept

    defaultLayout $ do
        setTitle "Villes du départment"
        $(widgetFile "department-cities")

-- | Handles GET requests giving the list of french department
getDepartmentListR :: Handler Html
getDepartmentListR = do
    departments <- runDB (selectList [] [] :: DB [Entity Departement])

    defaultLayout $ do
        setTitle "Liste des départements"
        $(widgetFile "department-list")
