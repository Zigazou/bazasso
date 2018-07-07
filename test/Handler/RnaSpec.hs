module Handler.RnaSpec (spec) where

import qualified Data.Text  as T
import           TestImport

spec :: Spec
spec = withApp $
    describe "Rna page" $ do
        it "loads the Rna page and checks it looks right" $ do
            get ByRnaR
            statusIs 200
            htmlAnyContain "h1" "Rechercher par numéro RNA"

        it "processes a good search request on W763005608" $ do
            get ByRnaR
            statusIs 200

            request $ do
                addToken

                addPostParam "rna-asso" "W763005608"

                setMethod "POST"
                setUrl ByRnaR

            statusIs 303
            redirection <- followRedirect
            case redirection of
                Left msg -> assertFailure (T.unpack msg)
                Right url -> do
                    get url
                    statusIs 200
                    htmlAnyContain "h1" "ÉCHELLE INCONNUE"

        it "processes a good search request on legacy 271S0271002017" $ do
            get ByRnaR
            statusIs 200

            request $ do
                addToken

                addPostParam "rna-asso" "271S0271002017"

                setMethod "POST"
                setUrl ByRnaR

            statusIs 303
            redirection <- followRedirect
            case redirection of
                Left msg -> assertFailure (T.unpack msg)
                Right url -> do
                    get url
                    statusIs 200
                    htmlAnyContain "h1" "ECURIE SPORT AUTO ANDELYSIEN"

        it "processes a bad search request on XXX" $ do
            get ByRnaR
            statusIs 200
            htmlNoneContain "p" "Aucune association"

            request $ do
                addToken

                addPostParam "search-asso" "XXX"

                setMethod "POST"
                setUrl ByRnaR

            statusIs 200
            htmlAnyContain "p" "Aucune association"
