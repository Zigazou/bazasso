{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Widgets.SearchResults ( searchResults ) where

import           Helpers.Clean      (clean)
import           Helpers.DateFormat (jjmmaaaa)
import           Helpers.Empty      (isEmpty, isSet)
import           Import

searchResultsNew :: [Entity Rnawaldec] -> Widget
searchResultsNew []       = [whamlet|<p>aucune association trouvée|]
searchResultsNew newassos = $(widgetFile "search-results-new")

searchResultsOld :: [Entity Rnaimport] -> Widget
searchResultsOld []       = [whamlet|<p>aucune association trouvée|]
searchResultsOld oldassos = $(widgetFile "search-results-old")

searchResults :: [Entity Rnawaldec] -> [Entity Rnaimport] -> Widget
searchResults newassos oldassos = do
    searchResultsNew newassos
    searchResultsOld oldassos
