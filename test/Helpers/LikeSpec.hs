module Helpers.LikeSpec (spec) where

import           TestImport

import           Helpers.Like

spec :: Spec
spec = withApp $ do
    describe "like" $ do
        it "finds Guadeloupe region" $ do
            guadeloupe <- runDB $ selectList [RegionLibelle `like` "del"]
                                             [LimitTo 1]

            let (Right guadeloupeId) = keyFromValues [PersistInt64 1]

            case guadeloupe of
                [Entity regionId _] -> assertEq "Guadeloupe is identified by 1"
                                                regionId
                                                guadeloupeId
                _ -> error "Unable to find Guadeloupe region"

        it "does not find XXX region" $ do
            noRegion <- runDB $ selectList [RegionLibelle `like` "XXX"]
                                           [LimitTo 1]

            assertEq "XXX region must not exist" noRegion []

    describe "startsLike" $ do
        it "finds Guadeloupe region" $ do
            guadeloupe <- runDB $ selectList [RegionLibelle `startsLike` "Gua"]
                                             [LimitTo 1]

            let (Right guadeloupeId) = keyFromValues [PersistInt64 1]

            case guadeloupe of
                [Entity regionId _] -> assertEq "Guadeloupe is identified by 1"
                                                regionId
                                                guadeloupeId
                _ -> error "Unable to find Guadeloupe region"

        it "does not find XXX region" $ do
            noRegion <- runDB $ selectList [RegionLibelle `startsLike` "XXX"]
                                           [LimitTo 1]

            assertEq "XXX region must not exist" noRegion []

    describe "endsLike" $ do
        it "finds Guadeloupe region" $ do
            guadeloupe <- runDB $ selectList [RegionLibelle `endsLike` "oupe"]
                                             [LimitTo 1]

            let (Right guadeloupeId) = keyFromValues [PersistInt64 1]

            case guadeloupe of
                [Entity regionId _] -> assertEq "Guadeloupe is identified by 1"
                                                regionId
                                                guadeloupeId
                _ -> error "Unable to find Guadeloupe region"

        it "does not find XXX region" $ do
            noRegion <- runDB $ selectList [RegionLibelle `endsLike` "XXX"]
                                           [LimitTo 1]

            assertEq "XXX region must not exist" noRegion []

    describe "match" $ do
        it "finds Expotec association" $ do
            expotec <- runDB $ selectList [RnawaldecIdent `match` "W763007801"]
                                          [LimitTo 1]

            case expotec of
                [_] -> return ()
                _   -> error "Unable to find Guadeloupe region"

        it "does not find XXX association" $ do
            noRegion <- runDB $ selectList [RnawaldecIdent `match` "XXX"]
                                           [LimitTo 1]

            case noRegion of
                [_] -> error "XXX association must not exist"
                _   -> return ()
