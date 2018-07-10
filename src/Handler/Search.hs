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

import           Helpers.GeneralTheme  (GeneralTheme, themesFilterNew,
                                        themesFilterOld)
import           Helpers.Like          (match, startsLike)
import           Import

import           Widgets.SearchForm    (SearchDept (OneDepartment),
                                        SearchFields (..), SearchForm (..),
                                        searchForm)
import           Widgets.SearchResults (searchResults)

import qualified Data.Text             as T

-- | Query to retrieve associations in the new database according to the
--   values entered in the search form
lookForNewAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForNewAssociations form = selectList (filters form) [LimitTo 1000]
    where
        filters :: SearchForm -> [Filter Rnawaldec]
        filters sf = concat [ keywordsF (searchString sf) (searchFields sf)
                            , themeF (searchTheme sf)
                            , deptF (searchDept sf)
                            ]

        keywordsF :: Text -> SearchFields -> [Filter Rnawaldec]
        keywordsF title TitleOnly = [RnawaldecTitre `match` T.toUpper title]
        keywordsF title AllFields =
            [ RnawaldecTitre `match` T.toUpper title ]
            ||. [ RnawaldecObjet `match` title ]
            ||. [ RnawaldecObjet `match` T.toUpper title ]

        themeF :: GeneralTheme -> [Filter Rnawaldec]
        themeF = themesFilterNew . return

        deptF :: SearchDept -> [Filter Rnawaldec]
        deptF (OneDepartment deptId _) =
            [RnawaldecAdrscodeinsee `startsLike` deptId]
        deptF _ = []

-- | Query to retrieve associations in the old database according to the
--   values entered in the search form
lookForOldAssociations :: DBparam SearchForm [Entity Rnaimport]
lookForOldAssociations form = selectList (filters form) [LimitTo 1000]
    where
        filters :: SearchForm -> [Filter Rnaimport]
        filters sf = concat [ keywordsF (searchString sf) (searchFields sf)
                            , themeF (searchTheme sf)
                            , deptF (searchDept sf)
                            ]

        keywordsF :: Text -> SearchFields -> [Filter Rnaimport]
        keywordsF title TitleOnly = [RnaimportTitre `match` T.toUpper title]
        keywordsF title AllFields =
            [ RnaimportTitre `match` T.toUpper title ]
            ||. [ RnaimportObjet `match` title ]
            ||. [ RnaimportObjet `match` T.toUpper title ]

        themeF :: GeneralTheme -> [Filter Rnaimport]
        themeF = themesFilterOld . return

        deptF :: SearchDept -> [Filter Rnaimport]
        deptF (OneDepartment deptId _) =
            [RnaimportAdrscodeinsee `startsLike` deptId]
        deptF _ = []

-- | Handles GET requests of the search page
getSearchR :: Handler Html
getSearchR = do
    (formWidget, formEnctype) <- generateFormPost searchForm

    let (newassos, oldassos) = ([], [])

    defaultLayout $ do
        let title = "Rechercher par mots-clés"
            method = GET
        setTitle title
        $(widgetFile "search")

-- | Handles POST requests of the search page
postSearchR :: Handler Html
postSearchR = do
    ((formResult, formWidget), formEnctype) <- runFormPost searchForm

    -- Look for association in the current database
    newassos <- case formResult of
        FormSuccess search -> runDB $ lookForNewAssociations search
        _                  -> return []

    -- Look for association in the legacy database
    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        _                  -> return []

    -- If there's only one result, redirect the user directly to the
    -- corresponding page.
    case (newassos, oldassos) of
        ([Entity newassoId _], []) ->
            redirect . NewAssociationR . unRnawaldecKey $ newassoId

        ([], [Entity oldassoId _]) ->
            redirect . OldAssociationR . unRnaimportKey $ oldassoId

        _ -> defaultLayout $ do
            let title = "Résultats de la recherche"
                method = POST
            setTitle title
            $(widgetFile "search")
