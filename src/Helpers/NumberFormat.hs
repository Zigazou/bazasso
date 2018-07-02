{- |
Module      :  NumberFormat
Description :  Formatting of number
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Formatting of number
-}
module Helpers.NumberFormat ( humanNumber ) where

import           Import

-- | Calculates the number of digits an integer would use
nbDigits :: Int -- ^ An integer
         -> Int -- ^ Number of digits it would use to print
nbDigits = (1 +) . floor . logBase 10 . (fromIntegral :: Int -> Double)

-- | Formats an integer so that it is easier to read. It adds a thin space to
--   materialize thousands, millions, billions...
humanNumber :: Int  -> String
humanNumber number = insertSpace (nbDigits number `mod` 3) (show number)
    where
        -- | Warning: the spaces are not common spaces but thin spaces!
        insertSpace :: Int -> String -> String
        insertSpace 0 []         = ""
        insertSpace 0 [a, b, c]  = [a, b, c]
        insertSpace 0 (a:b:c:as) = a : b : c : ' ' : insertSpace 0 as
        insertSpace 1 [a]        = [a]
        insertSpace 1 (a:as)     = a : ' ' : insertSpace 0 as
        insertSpace 2 [a, b]     = [a, b]
        insertSpace 2 (a:b:as)   = a : b : ' ' : insertSpace 0 as
        insertSpace _ _          = ""
