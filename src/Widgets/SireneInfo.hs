{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Widgets.SireneInfo ( sireneInfo ) where

import           Import

import           Data.Siret              (Siret, getSiren)
import           Helpers.EntitiesToMaybe (entitiesToMaybe)

getSirene :: DBparam Siret [Entity Sirene]
getSirene siret = selectList [SireneSiren ==. getSiren siret] [LimitTo 1]

sireneInfo :: Siret -> Widget
sireneInfo siret = do
    mSirene <- handlerToWidget $ runDB $ entitiesToMaybe <$> getSirene siret

    case mSirene of
        Nothing -> [whamlet|<span .text-muted>Pas de fiche Sirene associée|]
        Just sirene -> do
            mApet <- handlerToWidget . runDB . get $ sireneApet700 sirene
            mApen <- handlerToWidget . runDB . get $ sireneApen700 sirene
            mNj   <- handlerToWidget . runDB . get $ sireneNj sirene

            $(widgetFile "sirene-info")
