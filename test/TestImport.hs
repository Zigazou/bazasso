module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                (makeFoundation, makeLogWare)
import           ClassyPrelude              as X hiding (Handler, delete,
                                                  deleteBy)
import           Data.Either                (fromRight)
import qualified Data.Text                  as T
import           Database.Persist           as X hiding (get)
import           Database.Persist.Sql       (SqlPersistM, rawSql,
                                             runSqlPersistMPool, unSingle)
import           Foundation                 as X
import           Model                      as X
import           Network.Wai.Test           (simpleBody)
import           Test.Hspec                 as X
import           Test.QuickCheck            as X
import           Yesod.Auth                 as X
import           Yesod.Core                 (Yesod)
import           Yesod.Core.Handler         (RedirectUrl)
import           Yesod.Core.Unsafe          (fakeHandlerGetLogger)
import           Yesod.Default.Config2      (loadYamlSettings, useEnv)
import           Yesod.Test                 as X
import           Yesod.Test.TransversingCSS (findAttributeBySelector)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

isRelative :: Text -> Bool
isRelative url
    | T.take 7 url == "http://"  = False
    | T.take 8 url == "https://" = False
    | T.take 7 url == "mailto:"  = False
    | T.take 4 url == "tel:"     = False
    | otherwise                  = True

-- | Look for all links given a URL for the current page being visited.
getAllLinks :: YesodExample site [Text]
getAllLinks = withResponse $ \res -> do
    let currentHtml = simpleBody res
        links = fromRight [] $ findAttributeBySelector currentHtml "a" "href"
    return $ filter isRelative $ T.concat <$> links

-- | Given a URL, testAllLinks will check that every internal link gives a
--   200 anwser.
testAllLinks :: (RedirectUrl site url, Yesod site)
             => url
             -> SIO (YesodExampleData site) ()
testAllLinks startPage = do
    get startPage
    statusIs 200
    links <- getAllLinks

    forM_ links $ \oneLink -> do
        get startPage
        statusIs 200
        get oneLink
        statusIs 200
