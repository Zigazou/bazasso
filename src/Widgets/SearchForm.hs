{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  SearchForm
Description :  Generates and handles search form using a keyword and a theme.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Generates and handles a search form requiring a keyword and an optional theme
to be selected.
-}
module Widgets.SearchForm
    ( SearchForm(..)
    , searchForm
    ) where

import           Import

import           Helpers.GeneralTheme (gtOptionList)

-- | A structure holding the results of a search form.
data SearchForm = SearchForm
    { searchString :: Text       -- ^ Keyword to search for
    , searchThemes :: Maybe Text -- ^ optional theme
    }

-- | Settings for the keyword field.
textSettings :: FieldSettings master
textSettings = FieldSettings
    { fsLabel   = "Rechercher dans les titres des associations"
    , fsTooltip = Nothing
    , fsId      = Just "search-asso"
    , fsName    = Just "search-asso"
    , fsAttrs   = [ ("class", "form-control")
                    , ("placeholder", "Mots se trouvant dans le titre")
                    ]
    }

-- | Settings for the themes field.
selectSettings :: FieldSettings master
selectSettings = FieldSettings
    { fsLabel   = "Sélectionner les thèmes"
    , fsTooltip = Nothing
    , fsId      = Just "search-themes"
    , fsName    = Just "search-themes"
    , fsAttrs   = [ ("class", "large-select")]
    }

-- | The `searchForm` function generates and handles a search form based on a
--   keyword and an optional theme.
searchForm :: Html -- ^ Html to be placed before our generated html
           -> MForm Handler (FormResult SearchForm, Widget)
searchForm extra = do
    (searchRes, searchView) <- mreq textField textSettings Nothing
    (themeRes, themeView) <- mopt (selectFieldList gtOptionList)
                                  selectSettings Nothing

    let search = SearchForm <$> searchRes <*> themeRes
    let widget = [whamlet|
        #{extra}
        <div .input-group>
            ^{fvInput searchView}
            <span .input-group-btn>
                <button .btn.btn-default type="submit">
                    <i .glyphicon.glyphicon-search>
        <div>
            ^{fvInput themeView}
        |]

    return (search, widget)
