{- |
Module      :  Search
Description :  Handles search requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles search requests.
-}
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

-- | Query to retrieve associations in the new database according to the
--   values entered in the search form
lookForNewAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForNewAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForNewAssociations (SearchForm title (Just theme)) = selectList
    ( RnawaldecTitre `match` T.toUpper title : themesFilterNew [theme] )
    [LimitTo 1000]

-- | Query to retrieve associations in the old database according to the
--   values entered in the search form
lookForOldAssociations :: DBparam SearchForm [Entity Rnaimport]
lookForOldAssociations (SearchForm title Nothing) = selectList
    [RnaimportTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForOldAssociations (SearchForm title (Just theme)) = selectList
    ( RnaimportTitre `match` T.toUpper title : themesFilterOld [theme] )
    [LimitTo 1000]

-- | Handles GET requests of the search page
getSearchR :: Handler Html
getSearchR = do
    (formWidget, formEnctype) <- generateFormPost searchForm

    let (newassos, oldassos) = ([], [])

    defaultLayout $ do
        setTitle "Rechercher par mots-clés"
        $(widgetFile "search")

-- | Handles POST requests of the search page
postSearchR :: Handler Html
postSearchR = do
    ((formResult, formWidget), formEnctype) <- runFormPost searchForm

    newassos <- case formResult of
        FormSuccess search -> runDB $ lookForNewAssociations search
        _                  -> return []

    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        _                  -> return []

    defaultLayout $ do
        setTitle "Résultats de la recherche"
        $(widgetFile "search")
