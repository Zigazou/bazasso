{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Search
    ( getSearchR
    , postSearchR
    ) where

import Import
import Helpers.Like (match)

import Widgets.SearchForm (searchForm, SearchForm(..))
import Widgets.SearchResults (searchResults)

import qualified Data.Text as T

lookForNewAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForNewAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForNewAssociations (SearchForm title (Just theme)) = selectList
    ( RnawaldecTitre `match` T.toUpper title
    : ( [ RnawaldecObjetsocial1 >=. T.concat [theme, "000"]
        , RnawaldecObjetsocial1 <=. T.concat [theme, "999"]
        ] ||.
        [ RnawaldecObjetsocial2 >=. T.concat [theme, "000"]
        , RnawaldecObjetsocial2 <=. T.concat [theme, "999"]
        ]
      )
    )
    [LimitTo 1000]

lookForOldAssociations :: DBparam SearchForm [Entity Rnaimport]
lookForOldAssociations (SearchForm title Nothing) = selectList
    [RnaimportTitre `match` T.toUpper title]
    [LimitTo 1000]
lookForOldAssociations (SearchForm title (Just theme)) = selectList
    ( RnaimportTitre `match` T.toUpper title
    : ( [ RnaimportObjetsocial1 >=. T.concat [theme, "000"]
        , RnaimportObjetsocial1 <=. T.concat [theme, "999"]
        ] ||.
        [ RnaimportObjetsocial2 >=. T.concat [theme, "000"]
        , RnaimportObjetsocial2 <=. T.concat [theme, "999"]
        ]
      )
    )
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
        FormMissing -> return []
        FormFailure _ -> return []

    oldassos <- case formResult of
        FormSuccess search -> runDB $ lookForOldAssociations search
        FormMissing -> return []
        FormFailure _ -> return []

    defaultLayout $ do
        setTitle "Résultats de la recherche"
        $(widgetFile "search")
