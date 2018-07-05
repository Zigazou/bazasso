module Handler.HomeSpec (spec) where

import           Data.Either                (fromRight)
import qualified Data.Text                  as T
import           Network.Wai.Test           (simpleBody)
import           TestImport
import           Yesod.Test.TransversingCSS (findAttributeBySelector)

getAllLinks :: YesodExample site [Text]
getAllLinks = withResponse $ \res -> do
    let links = fromRight [] findAttributeBySelector (simpleBody res) "a" "href"
    return $ T.concat <$> links

spec :: Spec
spec = withApp $
    describe "Homepage" $ do
        it "loads the index and checks it looks right" $ do
            get HomeR
            statusIs 200
            htmlAnyContain "h1" "Bazasso"

        it "checks all links" $ do
            get HomeR
            statusIs 200
            links <- getAllLinks

            forM_ links $ \oneLink -> do
                get HomeR
                statusIs 200
                get oneLink
                statusIs 200
