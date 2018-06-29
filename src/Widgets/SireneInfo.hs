{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
    
    [whamlet|
        $maybe sirene <- mSirene
            <p>
                <strong>Activité principale de l'établissement&nbsp;:
                #{sireneApet700 sirene}
            <p>
                <strong>Activité principale de l'entreprise&nbsp;:
                #{sireneApen700 sirene}
            <p>
                <strong>Tranche d'effectif salarié de l'établissement&nbsp;:
                #{sireneTefet sirene}
            <p>
                <strong>Tranche d'effectif salarié de l'entreprise&nbsp;:
                #{sireneTefen sirene}
        $nothing
            <span .text-muted>Pas de fiche Sirene associée
    |]
