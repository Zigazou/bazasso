{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Association
    ( getOldAssociationR
    , getNewAssociationR
    ) where

import Import
import Helpers.Clean (clean)
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (match)
import Helpers.EntitiesToMaybe (entitiesToMaybe)
import Helpers.Empty (isSet)

import Database.Persist.Class (toPersistValue)
import Database.Persist.Sql (rawSql)

import Widgets.SireneInfo (sireneInfo)

import qualified Data.Text as T

getNewAssociation :: DBparam Text [Entity Rnawaldec]
getNewAssociation txt = selectList [RnawaldecIdent `match` txt] [LimitTo 1]

getOldAssociation :: DBparam Text [Entity Rnaimport]
getOldAssociation txt = selectList [RnaimportIdent `match` txt] [LimitTo 1]

getAnnonces :: DBparam Text [Entity Joannonce]
getAnnonces txt = selectList [JoannonceWaldec ==. txt] [LimitTo 1000]

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

themeIdentifiedBy :: JothemeId -> Widget
themeIdentifiedBy ident = do
    mTheme <- handlerToWidget . runDB . get $ ident

    [whamlet|
        $maybe theme <- mTheme
            #{jothemeLibelle theme}
        $nothing
            <span .text-muted>non renseigné
    |]

typeAvisIdentifiedBy :: JotypeavisId -> Widget
typeAvisIdentifiedBy ident = do
    mTypeAvis <- handlerToWidget . runDB . get $ ident

    [whamlet|
        $maybe typeAvis <- mTypeAvis
            #{jotypeavisLibelle typeAvis}
        $nothing
            <span .text-muted>non renseigné
    |]

isWaldec ::Text -> Bool
isWaldec txt
    | T.null txt = False
    | T.head txt /= 'W' = False
    | T.length txt /= 10 = False
    | otherwise = True

getNewAssociationR :: Text -> Handler Html
getNewAssociationR waldec = do
    mAssociation <- runDB $ entitiesToMaybe <$> getNewAssociation waldec

    annonces <- runDB $ getAnnonces waldec

    defaultLayout $ do
        setTitle "Fiche association"
        $(widgetFile "association-new")

getOldAssociationR :: Text -> Handler Html
getOldAssociationR ident = do
    mAssociation <- runDB $ entitiesToMaybe <$> getOldAssociation ident

    defaultLayout $ do
        setTitle "Fiche association"
        $(widgetFile "association-old")
