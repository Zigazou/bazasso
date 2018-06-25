{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Search
    ( postSearchR
    ) where

import Import
import Helpers.Empty (clean, Empty(..))
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (like, startsLike)

import Widgets.SearchForm (searchForm, SearchForm(..))

import qualified Data.Text as T
import Data.List (foldl)

lookForAssociations :: DBparam SearchForm [Entity Rnawaldec]
lookForAssociations (SearchForm title Nothing) = selectList
    [RnawaldecTitre `like` (T.toUpper title)]
    [LimitTo 1000]
lookForAssociations (SearchForm title (Just themes)) = selectList
    ([RnawaldecTitre `like` (T.toUpper title)] ++ themesFilter themes)
    [LimitTo 1000]
    where
        addTheme [] b = [RnawaldecObjetsocial1 `startsLike` b]
                    ||. [RnawaldecObjetsocial2 `startsLike` b]
        addTheme a b = a ||. addTheme [] b
        themesFilter ts = foldl addTheme [] ts

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
