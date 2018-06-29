{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.SireneInfo ( sireneInfo ) where

import Import

import Helpers.EntitiesToMaybe (entitiesToMaybe)
import Data.Siret (Siret, getSiren)

getSirene :: DBparam Siret [Entity Sirene]
getSirene siret = selectList [SireneSiren ==. getSiren siret]
                             [LimitTo 1]

sireneInfo :: Siret -> Widget
sireneInfo siret = do
    mSirene <- handlerToWidget $ runDB $ entitiesToMaybe <$> getSirene siret

    case mSirene of
        Nothing -> [whamlet|<span .text-muted>Pas de fiche Sirene associée|]
        Just sirene -> do
            mApet <- handlerToWidget . runDB . get $ sireneApet700 sirene
            mApen <- handlerToWidget . runDB . get $ sireneApen700 sirene
            mNj <- handlerToWidget . runDB . get $ sireneNj sirene

            $(widgetFile "sirene-info")
