{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Association
    ( getAssociationR
    ) where

import Import
import Helpers.Empty (clean)
import Helpers.DateFormat (jjmmaaaa)
import Helpers.Like (match)

import Database.Persist.Class (toPersistValue)
import Database.Persist.Sql (rawSql)

longPosition :: Text -> Text
longPosition "A" = "active"
longPosition "D" = "dissoute"
longPosition "S" = "supprimée"
longPosition _ = ""

longGroupement :: Text -> Text
longGroupement "S" = "simple"
longGroupement "U" = "union"
longGroupement "F" = "fédération"
longGroupement _ = ""

longNature :: Text -> Text
longNature "" = ""
longNature a = a

getAssociation :: DBparam Text [Entity Rnawaldec]
getAssociation txt = selectList [RnawaldecIdent `match` txt] [LimitTo 1]

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

entitiesToMaybe:: [Entity a] -> Maybe a
entitiesToMaybe [Entity _ x] = Just x
entitiesToMaybe _ = Nothing

getTheme :: DBparam Text [Entity Jotheme]
getTheme ident = selectList [JothemeTheme ==. ident] [LimitTo 1]

themeIdentifiedBy :: Text -> Widget
themeIdentifiedBy ident = do
    mTheme <- handlerToWidget $ runDB $ getTheme ident >>= return . entitiesToMaybe

    [whamlet|
        $maybe theme <- mTheme
            #{jothemeLibelle theme}
        $nothing
            <span .text-muted>non renseigné
    |]

getType :: DBparam Int [Entity Jotypeavis]
getType ident = selectList [JotypeavisIdent ==. ident] [LimitTo 1]

typeAvisIdentifiedBy :: Int -> Widget
typeAvisIdentifiedBy ident = do
    mTypeAvis <- handlerToWidget $ runDB $ getType ident >>= return . entitiesToMaybe

    [whamlet|
        $maybe typeAvis <- mTypeAvis
            #{jotypeavisLibelle typeAvis}
        $nothing
            <span .text-muted>non renseigné
    |]

getAssociationR :: Text -> Handler Html
getAssociationR waldec = do
    mAssociation <- runDB $ getAssociation waldec >>= return . entitiesToMaybe

    annonces <- runDB $ getAnnonces waldec

    defaultLayout $ do
        setTitle "Fiche association"
        $(widgetFile "association")
