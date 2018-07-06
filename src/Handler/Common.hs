{- |
Module      :  Common
Description :  Handles static requests
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Handles static requests.
-}
module Handler.Common where

import           Data.FileEmbed (embedFile)
import           Import

-- | Defines a default cache period for everything related to favicon.
cacheIcon :: HandlerFor App ()
cacheIcon = cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month

-- | Return the favicon file
getFaviconIcoR :: Handler TypedContent
getFaviconIcoR = do
    cacheIcon
    return $ TypedContent "image/x-icon"
           $ toContent $(embedFile "config/favicon/favicon.ico")

-- | Return the favicon file
getFavicon16x16R :: Handler TypedContent
getFavicon16x16R = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/favicon-16x16.png")

-- | Return the favicon file
getFavicon32x32R :: Handler TypedContent
getFavicon32x32R = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/favicon-32x32.png")

getSiteWebManifestR :: Handler TypedContent
getSiteWebManifestR = do
    cacheIcon
    return $ TypedContent "text/plain"
           $ toContent $(embedFile "config/favicon/site.webmanifest")

getBrowserConfigXmlR :: Handler TypedContent
getBrowserConfigXmlR = do
    cacheIcon
    return $ TypedContent "application/xml"
           $ toContent $(embedFile "config/favicon/browserconfig.xml")

getAndroidChrome192x192R :: Handler TypedContent
getAndroidChrome192x192R = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/android-chrome-192x192.png")

getAndroidChrome512x512R :: Handler TypedContent
getAndroidChrome512x512R = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/android-chrome-512x512.png")

getAppleTouchIconR :: Handler TypedContent
getAppleTouchIconR = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/apple-touch-icon.png")

getSafariPinnedTabR :: Handler TypedContent
getSafariPinnedTabR = do
    cacheIcon
    return $ TypedContent "image/svg+xml"
           $ toContent $(embedFile "config/favicon/safari-pinned-tab.svg")

getMSTile150x150R :: Handler TypedContent
getMSTile150x150R = do
    cacheIcon
    return $ TypedContent "image/png"
           $ toContent $(embedFile "config/favicon/mstile-150x150.png")

-- | Return the robots.txt file
getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
