{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.NumberFormatSpec (spec) where

import           TestImport

import           Helpers.NumberFormat

spec :: Spec
spec = do
    describe "formatting int" $ do
        it "gives a longer or same length string than show" $ property $
            \x -> (length . humanNumber) x >= (length . show) x

        it "gives a string no longer than four third show" $ property $
            \x -> (length . humanNumber) x
                  <= ((`div` 3) . (4 *) .length . show) x

        it "is the same as show if thin spaces are removed" $ property $
            \x -> (filter (/= ' ') . humanNumber) x == show x
