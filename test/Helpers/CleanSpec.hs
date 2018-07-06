module Helpers.CleanSpec (spec) where

import           TestImport

import qualified Data.Text                     as T
import           Helpers.Clean
import           Text.Blaze.Html.Renderer.Text (renderHtml)

spec :: Spec
spec = withApp $
    describe "clean" $ do
        it "gives XXX for XXX" $ do
            let xxx = clean id ("XXX" :: Text)
            assertEq "XXX renders as XXX" (renderHtml xxx) "XXX"

        it "gives xxx for XXX when toUppered" $ do
            let xxx = clean T.toUpper ("xxx" :: Text)
            assertEq "xxx renders as XXX when toUppered" (renderHtml xxx) "XXX"
