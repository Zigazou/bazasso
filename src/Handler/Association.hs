{- |
Module      :  Association
Description :  Handles association page requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles association page requests.
-}
module Handler.Association
    ( getOldAssociationR
    , getNewAssociationR
    ) where

import           Helpers.Clean           (clean)
import           Helpers.DateFormat      (jjmmaaaa)
import           Helpers.Empty           (isSet)
import           Helpers.EntitiesToMaybe (entitiesToMaybe)
import           Helpers.Like            (match)
import           Import

import           Database.Persist.Class  (toPersistValue)
import           Database.Persist.Sql    (rawSql)

import           Widgets.SireneInfo      (sireneInfo)

import qualified Data.Text               as T

-- | Query to retrieve an association from the new database given a Waldec
--   number.
getNewAssociation :: DBparam Text [Entity Rnawaldec]
getNewAssociation txt = selectList [RnawaldecIdent `match` txt] [LimitTo 1]

-- | Query to retrieve an association from the old database given a RNA number.
getOldAssociation :: DBparam Text [Entity Rnaimport]
getOldAssociation txt = selectList [RnaimportIdent `match` txt] [LimitTo 1]

-- | Query to retrieve JO announces for an association given its Waldec number.
getAnnonces :: DBparam Text [Entity Joannonce]
getAnnonces txt = selectList [JoannonceWaldec ==. txt] [LimitTo 1000]

-- | Query to retrieve themes associated to a JO announce.
getThemes :: DBparam (Int, Int) [Entity Jotheme]
getThemes (numparution, numannonce) = rawSql query values
    where
        query :: Text
        query = "SELECT ?? \
                \FROM jotheme \
                \INNER JOIN joanntheme ON joanntheme.theme = jotheme.theme \
                \WHERE joanntheme.num_parution = ? \
                \AND joanntheme.num_annonce = ? \
                \LIMIT 10"

        values :: [PersistValue]
        values = toPersistValue <$> [numparution, numannonce]

-- | Query to retrieve themes associated to a JO announce.
themesOf :: Joannonce -> Widget
themesOf annonce = do
    themes <- handlerToWidget $ runDB $ getThemes
              (joannonceNumparution annonce, joannonceNumannonce annonce)
    [whamlet|
        $if null themes
            <span .text-muted>non renseigné
        $else
            <ul>
                $forall Entity _ theme <- themes
                    <li>#{jothemeLibelle theme}
    |]

-- | Display theme name given its identifier
themeIdentifiedBy :: JothemeId -> Widget
themeIdentifiedBy ident = do
    mTheme <- handlerToWidget . runDB . get $ ident

    [whamlet|
        $maybe theme <- mTheme
            #{jothemeLibelle theme}
        $nothing
            <span .text-muted>non renseigné
    |]

-- | Display type avis given its identifier
typeAvisIdentifiedBy :: JotypeavisId -> Widget
typeAvisIdentifiedBy ident = do
    mTypeAvis <- handlerToWidget . runDB . get $ ident

    [whamlet|
        $maybe typeAvis <- mTypeAvis
            #{jotypeavisLibelle typeAvis}
        $nothing
            <span .text-muted>non renseigné
    |]

-- | Tells if a string is a Waldec number or not
isWaldec ::Text -> Bool
isWaldec txt | T.null txt         = False
             | T.head txt /= 'W'  = False
             | T.length txt /= 10 = False
             | otherwise          = True

-- | Handles requests for association from the new database
getNewAssociationR :: Text         -- ^ A Waldec number
                   -> Handler Html -- ^ Request handler
getNewAssociationR waldec = do
    mAssociation <- runDB $ entitiesToMaybe <$> getNewAssociation waldec

    annonces <- runDB $ getAnnonces waldec

    defaultLayout $ do
        setTitle "Fiche association"
        $(widgetFile "association-new")

-- | Handles requests for association from the old database
getOldAssociationR :: Text         -- ^ An old association number
                   -> Handler Html -- ^ Request handler
getOldAssociationR ident = do
    mAssociation <- runDB $ entitiesToMaybe <$> getOldAssociation ident

    defaultLayout $ do
        setTitle "Fiche association"
        $(widgetFile "association-old")
