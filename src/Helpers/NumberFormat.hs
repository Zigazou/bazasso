{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.NumberFormat ( humanNumber ) where

import Import

humanNumber :: Int  -> String
humanNumber number = insertSpace (nbDigits `mod` 3) (show number)
    where
        nbDigits :: Int
        nbDigits = 1 + floor (logBase 10 (fromIntegral number :: Double))

        insertSpace :: Int -> String -> String
        insertSpace 0 [] = ""
        insertSpace 0 [a, b, c] = [a, b, c]
        insertSpace 0 (a:b:c:as) = a : b : c : ' ' : insertSpace 0 as
        insertSpace 1 [a] = [a]
        insertSpace 1 (a:as) = a : ' ' : insertSpace 0 as
        insertSpace 2 [a, b] = [a, b]
        insertSpace 2 (a:b:as) = a : b : ' ' : insertSpace 0 as
        insertSpace _ _ = ""
