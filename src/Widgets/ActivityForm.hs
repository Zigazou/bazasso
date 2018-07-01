{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Widgets.ActivityForm
    ( ActivityForm(..)
    , activityForm
    ) where

import           Import

import           Helpers.GeneralTheme (gtOptionList)

data ActivityForm = ActivityForm
    { activityInsee  :: Text
    , activityThemes :: [Text]
    }

activityForm :: Text -> Html -> MForm Handler (FormResult ActivityForm, Widget)
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

    where
        selectSettings  = FieldSettings
            { fsLabel   = "Sélectionner les thèmes"
            , fsTooltip = Nothing
            , fsId      = Just "search-themes"
            , fsName    = Just "search-themes"
            , fsAttrs   = [("class", "big-select")]
            }
