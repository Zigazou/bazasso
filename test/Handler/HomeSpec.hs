module Handler.HomeSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $
    describe "Homepage" $
        it "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAnyContain "h1" "Bazasso"
