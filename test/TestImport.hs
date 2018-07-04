module TestImport
    ( module TestImport
    , module X
    ) where

import           Application           (makeFoundation, makeLogWare)
import           ClassyPrelude         as X hiding (Handler, delete, deleteBy)
import           Database.Persist      as X hiding (get)
import           Database.Persist.Sql  (SqlPersistM, rawSql, runSqlPersistMPool,
                                        unSingle)
import           Foundation            as X
import           Model                 as X
import           Test.Hspec            as X
import           Test.QuickCheck       as X
import           Yesod.Auth            as X
import           Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import           Yesod.Default.Config2 (loadYamlSettings, useEnv)
import           Yesod.Test            as X

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
