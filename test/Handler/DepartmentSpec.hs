module Handler.DepartmentSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
    describe "Cities of a Seine-Maritime" $ do
        let (Right seinemaritimeId) = keyFromValues [PersistInt64 76]
        it "gives a 200" $ do
            get (DepartmentCitiesR seinemaritimeId)
            statusIs 200
            htmlAnyContain "li a" "Rouen"
            htmlNoneContain "li a" "Strasbourg"

        it "contains no dead link" $
            testAllLinks (DepartmentCitiesR seinemaritimeId)

    describe "List of french departments" $ do
        it "gives a 200" $ do
            get DepartmentListR
            statusIs 200
            htmlAnyContain "li a" "Seine-Maritime"

        it "contains no dead link" $
            testAllLinks DepartmentListR
