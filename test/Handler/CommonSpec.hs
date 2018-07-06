module Handler.CommonSpec (spec) where

import           TestImport

-- | URLs to check with their content-type
urlsToCheck :: [(Route App, ByteString)]
urlsToCheck =
    [ ( SiteWebManifestR, "text/plain" )
    , ( BrowserConfigXmlR, "application/xml" )
    , ( FaviconIcoR, "image/x-icon" )
    , ( Favicon32x32R, "image/png" )
    , ( Favicon16x16R, "image/png" )
    , ( AndroidChrome192x192R, "image/png" )
    , ( AndroidChrome512x512R, "image/png" )
    , ( AppleTouchIconR, "image/png" )
    , ( SafariPinnedTabR, "image/svg+xml" )
    , ( MSTile150x150R, "image/png" )
    , ( RobotsR, "text/plain; charset=utf-8" )
    ]

spec :: Spec
spec = withApp $
    forM_ urlsToCheck $ \(url, mimetype) ->
        describe (show url) $ do
            it "gives a 200" $ do
                get url
                statusIs 200

            it "has correct mime type" $ do
                get url
                assertHeader "Content-Type" mimetype
