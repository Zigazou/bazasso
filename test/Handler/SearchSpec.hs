module Handler.SearchSpec (spec) where

import qualified Data.Text  as T
import           TestImport

spec :: Spec
spec = withApp $
    describe "Search page" $ do
        it "loads the search page and checks it looks right" $ do
            get SearchR
            statusIs 200
            htmlAnyContain "h1" "Rechercher par mots-clés"
            htmlNoneContain "h1" "Base Waldec"
{- TODO: fix these tests because they hang indefinitely for no apparent reason
        it "processes a good search request on EXPOTEC" $ do
            get SearchR
            statusIs 200

            request $ do
                addToken

                addPostParam "search-asso" "expotec"
                addPostParam "search-theme" "1"
                addPostParam "search-dept" "1"
                addPostParam "search-fields" "1"

                setMethod "POST"
                setUrl SearchR

            statusIs 303
            redirection <- followRedirect
            case redirection of
                Left msg -> assertFailure (T.unpack msg)
                Right url -> do
                    get url
                    statusIs 200
                    htmlAnyContain "h2" "EXPOTEC 103"

        it "processes a bad search request on EXPOTEC" $ do
            get SearchR
            statusIs 200

            request $ do
                addToken

                addPostParam "search-asso" "expotec"
                addPostParam "search-theme" "2"
                addPostParam "search-dept" "1"
                addPostParam "search-fields" "1"

                setMethod "POST"
                setUrl SearchR

            statusIs 200
            htmlNoneContain "a" "EXPOTEC 103"
-}
