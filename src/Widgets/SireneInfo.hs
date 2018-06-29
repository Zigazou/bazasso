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
            <h4>Activité principale
            <ul>
                <li>
                    <strong>Établissement&nbsp;:
                    #{sireneApet700 sirene}
                <li>
                    <strong>Entreprise&nbsp;:
                    #{sireneApen700 sirene}

            <h4>Tranche d'effectif
            <ul>
                <li>
                    <strong>Établissement&nbsp;:
                    #{sireneTefet sirene}
                <li>
                    <strong>Entreprise&nbsp;:
                    #{sireneTefen sirene}
        $nothing
            <span .text-muted>Pas de fiche Sirene associée
    |]
