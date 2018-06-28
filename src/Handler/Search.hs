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
import Helpers.Empty (Empty(..))
import Helpers.Clean (clean)
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (match)

import Widgets.SearchForm (searchForm, SearchForm(..))

import qualified Data.Text as T
import Data.List (foldl)

lookForNewAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForNewAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `match` (T.toUpper title)]
    [LimitTo 1000]
lookForNewAssociations (SearchForm title (Just themes)) = selectList
    ([RnawaldecTitre `match` (T.toUpper title)] ++ themesFilter themes)
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

lookForOldAssociations :: DBparam SearchForm [Entity Rnaimport]
lookForOldAssociations (SearchForm title Nothing) = selectList
    [RnaimportTitre `match` (T.toUpper title)]
    [LimitTo 1000]
lookForOldAssociations (SearchForm title (Just themes)) = selectList
    ([RnaimportTitre `match` (T.toUpper title)] ++ themesFilter themes)
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
