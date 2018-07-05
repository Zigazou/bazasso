module Handler.AssociationSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
    describe "Expotec" $

        it "gives a 200" $ do
            get (NewAssociationR "W763007801")
            statusIs 200
            htmlAnyContain "h2" "EXPOTEC 103"

    describe "Compagnie provisoire" $
        it "gives a 200" $ do
            get (OldAssociationR "601P0601014236")
            statusIs 200
            htmlAnyContain "h1" "COMPAGNIE PROVISOIRE"

