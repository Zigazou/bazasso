module Helpers.EntitiesToMaybeSpec (spec) where

import           TestImport

import           Helpers.EntitiesToMaybe

spec :: Spec
spec = withApp $
    describe "entitiesToMaybe" $ do
        it "gives Nothing if list is empty" $
            assertEq "should be nothing"
                     (entitiesToMaybe ([] :: [Entity Region]))
                     Nothing

        it "gives Just the first element for a list of one element" $ do
            test1 <- runDB (selectList [] [LimitTo 1] :: DB [Entity Region])
            return (entitiesToMaybe test1 `shouldSatisfy` isJust)

        it "gives Just the first element for a list of two elements" $ do
            test2 <- runDB (selectList [] [LimitTo 3] :: DB [Entity Region])
            return (entitiesToMaybe test2 `shouldSatisfy` isJust)
