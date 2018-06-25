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
import Helpers.Empty (clean, Empty(..))
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (match)

import Widgets.SearchForm (searchForm, SearchForm(..))

import qualified Data.Text as T
import Data.List (foldl)

lookForAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `match` (T.toUpper title)]
    [LimitTo 1000]
lookForAssociations (SearchForm title (Just themes)) = selectList
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
        themesFilter ts = foldl addTheme [] ts

getSearchR :: Handler Html
getSearchR = do
    (formWidget, formEnctype) <- generateFormPost searchForm

    let associations = []

    defaultLayout $ do
        setTitle "Rechercher par mots-clés"
        $(widgetFile "search")

postSearchR :: Handler Html
postSearchR = do
    ((formResult, formWidget), formEnctype) <- runFormPost searchForm

    associations <- case formResult of
        FormSuccess search -> runDB $ lookForAssociations search
        FormMissing -> return []
        FormFailure _ -> return []

    defaultLayout $ do
        setTitle "Résultats de la recherche"
        $(widgetFile "search")
