{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home
    ( getHomeR
    ) where

import Import
import Widgets.SearchForm (searchForm)

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost searchForm

    defaultLayout $ do
        setTitle "Bazasso"
        $(widgetFile "homepage")