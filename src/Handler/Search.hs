{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Search
    ( getSearchR
    , postSearchR
    ) where

import           Helpers.GeneralTheme  (themesFilterNew, themesFilterOld)
import           Helpers.Like          (match)
import           Import

import           Widgets.SearchForm    (SearchForm (..), searchForm)
import           Widgets.SearchResults (searchResults)

import qualified Data.Text             as T

lookForNewAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForNewAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForNewAssociations (SearchForm title (Just theme)) = selectList
    ( RnawaldecTitre `match` T.toUpper title : themesFilterNew [theme] )
    [LimitTo 1000]

lookForOldAssociations :: DBparam SearchForm [Entity Rnaimport]
lookForOldAssociations (SearchForm title Nothing) = selectList
    [RnaimportTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForOldAssociations (SearchForm title (Just theme)) = selectList
    ( RnaimportTitre `match` T.toUpper title : themesFilterOld [theme] )
    [LimitTo 1000]

getSearchR :: Handler Html
getSearchR = do
    (formWidget, formEnctype) <- generateFormPost searchForm

    let (newassos, oldassos) = ([], [])

    defaultLayout $ do
        setTitle "Rechercher par mots-clés"
        $(widgetFile "search")

postSearchR :: Handler Html
postSearchR = do
    ((formResult, formWidget), formEnctype) <- runFormPost searchForm

    newassos <- case formResult of
        FormSuccess search -> runDB $ lookForNewAssociations search
        FormMissing        -> return []
        FormFailure _      -> return []

    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        FormMissing        -> return []
        FormFailure _      -> return []

    defaultLayout $ do
        setTitle "Résultats de la recherche"
        $(widgetFile "search")
