module Handler.DepartmentSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
    describe "Cities of a Seine-Maritime" $
        it "gives a 200" $ do
            let (Right seinemaritimeId) = keyFromValues [PersistInt64 76]
            get (DepartmentCitiesR seinemaritimeId)
            statusIs 200
            htmlAnyContain "li a" "Rouen"
            htmlNoneContain "li a" "Strasbourg"

    describe "List of french departments" $
        it "gives a 200" $ do
            get DepartmentListR
            statusIs 200
            htmlAnyContain "li a" "Seine-Maritime"
