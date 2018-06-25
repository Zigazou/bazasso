{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.SearchForm
    ( SearchForm(..)
    , searchForm
    ) where

import Import

import Helpers.GeneralTheme (gtOptionList)

data SearchForm = SearchForm
    { searchString :: Text
    , searchThemes :: Maybe [Text]
    }

searchForm :: Html -> MForm Handler (FormResult SearchForm, Widget)
searchForm extra = do
    (searchRes, searchView) <- mreq textField textSettings Nothing
    (themesRes, themesView) <- mopt (multiSelectFieldList gtOptionList)
                                    selectSettings Nothing

    let search = SearchForm <$> searchRes <*> themesRes
    let widget = [whamlet|
        #{extra}
        <div .input-group>
            ^{fvInput searchView}
            <span .input-group-btn>
                <button .btn.btn-default type="submit">
                    <i .glyphicon.glyphicon-search>
        <div>
            ^{fvInput themesView}
        |]

    return (search, widget)

    where
        textSettings = FieldSettings
            { fsLabel = "Rechercher dans les titres des associations"
            , fsTooltip = Nothing
            , fsId = Just "search-asso"
            , fsName = Just "search-asso"
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "Mots se trouvant dans le titre")
                ]
            }

        selectSettings = FieldSettings
            { fsLabel = "Sélectionner les thèmes"
            , fsTooltip = Nothing
            , fsId = Just "search-themes"
            , fsName = Just "search-themes"
            , fsAttrs = [("class", "big-select")]
            }
