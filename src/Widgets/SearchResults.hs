{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  SearchResults
Description :  Search results widget.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Widget displaying the results of a search query. Results come from the old and
new association databases.
-}
module Widgets.SearchResults ( searchResults ) where

import           Helpers.Clean      (clean)
import           Helpers.DateFormat (jjmmaaaa)
import           Helpers.Empty      (isEmpty, isSet)
import           Import

-- | Display results from the new database.
searchResultsNew :: [Entity Rnawaldec] -- ^ Associations from the new database
                 -> Widget             -- ^ Widget to insert in a page
searchResultsNew []       = [whamlet|<p>aucune association trouvée|]
searchResultsNew newassos = $(widgetFile "search-results-new")

-- | Display results from the old database.
searchResultsOld :: [Entity Rnaimport] -- ^ Associations from the old database
                 -> Widget             -- ^ Widget to insert in a page
searchResultsOld []       = [whamlet|<p>aucune association trouvée|]
searchResultsOld oldassos = $(widgetFile "search-results-old")

-- | Display results from the new and old databases.
searchResults :: [Entity Rnawaldec] -- ^ Associations from the new database
              -> [Entity Rnaimport] -- ^ Associations from the old database
              -> Widget             -- ^ Widget to insert in a page
searchResults newassos oldassos = do
    searchResultsNew newassos
    searchResultsOld oldassos
