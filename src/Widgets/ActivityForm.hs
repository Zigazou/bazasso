{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  ActivityForm
Description :  Generates and handles search form using city and themes.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Generates and handles a search form requiring a city and themes to be selected.
-}
module Widgets.ActivityForm
    ( ActivityForm(..)
    , activityForm
    ) where

import           Import

import           Helpers.GeneralTheme (gtOptionList)

-- | A structure holding the results of an activity form.
data ActivityForm = ActivityForm
    { activityInsee  :: Text   -- ^ Insee code (specifies a city in France)
    , activityThemes :: [Text] -- ^ List of selected activity themes
    }

-- | Settings for the themes field.
selectSettings :: FieldSettings master
selectSettings  = FieldSettings
    { fsLabel   = "Sélectionner les thèmes"
    , fsTooltip = Nothing
    , fsId      = Just "search-themes"
    , fsName    = Just "search-themes"
    , fsAttrs   = [("class", "big-select")]
    }

-- | The `activityForm` function generates and handles a search form based on an
--   Insee code.
activityForm :: Text -- ^ Insee code (5 characters)
             -> Html -- ^ Html to be placed before our generated html
             -> MForm Handler (FormResult ActivityForm, Widget)
activityForm insee extra = do
    (inseeRes, inseeView) <- mreq hiddenField "" (Just insee)
    (themesRes, themesView) <- mreq (multiSelectFieldList gtOptionList)
                                    selectSettings Nothing

    let search = ActivityForm <$> inseeRes <*> themesRes
    let widget = [whamlet|
        #{extra}
        ^{fvInput inseeView}
        <div>
            ^{fvInput themesView}
        <div>
            <button .btn.btn-default type="submit">
                Rechercher
        |]

    return (search, widget)
