module Handler.SearchSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $
    describe "Search page" $ do
        it "loads the search page and checks it looks right" $ do
            get SearchR
            statusIs 200
            htmlAnyContain "h1" "Résultats de la recherche"
            htmlNoneContain "h1" "Base Waldec"

        it "processes a good search request on EXPOTEC" $ do
            get SearchR
            statusIs 200

            request $ do
                addToken

                addPostParam "search-asso" "expotec"
                addPostParam "search-themes" ""

                setMethod "POST"
                setUrl SearchR

            statusIs 200
            htmlAnyContain "a" "EXPOTEC 103"

        it "processes a bad search request on EXPOTEC" $ do
            get SearchR
            statusIs 200

            request $ do
                addToken

                addPostParam "search-asso" "expotec"
                addPostParam "search-themes" "1"

                setMethod "POST"
                setUrl SearchR

            statusIs 200
            htmlNoneContain "a" "EXPOTEC 103"
