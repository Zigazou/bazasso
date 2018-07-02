{- |
Module      :  SireneInfo
Description :  Display information from the Sirene database.
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Display information from the Sirene database.
-}
module Widgets.SireneInfo ( sireneInfo ) where

import           Import

import           Data.Siret              (Siret, getSiren)
import           Helpers.EntitiesToMaybe (entitiesToMaybe)

-- | Generates a query to retrieve a Sirene entry given a Siret number.
getSirene :: DBparam Siret [Entity Sirene]
getSirene siret = selectList [SireneSiren ==. getSiren siret] [LimitTo 1]

-- | Display information from the Sirene database given a Siret number.
sireneInfo :: Siret  -- ^ The Siret number identifying the society
           -> Widget -- ^ The widget to insert in a page
sireneInfo siret = do
    mSirene <- handlerToWidget $ runDB $ entitiesToMaybe <$> getSirene siret

    case mSirene of
        Nothing -> [whamlet|<span .text-muted>Pas de fiche Sirene associée|]
        Just sirene -> do
            mApet <- handlerToWidget . runDB . get $ sireneApet700 sirene
            mApen <- handlerToWidget . runDB . get $ sireneApen700 sirene
            mNj   <- handlerToWidget . runDB . get $ sireneNj sirene

            $(widgetFile "sirene-info")
