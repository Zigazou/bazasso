{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Widgets.SearchResults ( searchResults ) where

import Import
import Helpers.Clean (clean)
import Helpers.Empty (isEmpty, isSet)
import Helpers.DateFormat (jjmmaaaa)

searchResultsNew :: [Entity Rnawaldec] -> Widget
searchResultsNew [] = [whamlet|<p>aucune association trouvée|]
searchResultsNew newassos = $(widgetFile "search-results-new")

searchResultsOld :: [Entity Rnaimport] -> Widget
searchResultsOld [] = [whamlet|<p>aucune association trouvée|]
searchResultsOld oldassos = $(widgetFile "search-results-old")

searchResults :: [Entity Rnawaldec] -> [Entity Rnaimport] -> Widget
searchResults newassos oldassos = do
    searchResultsNew newassos
    searchResultsOld oldassos