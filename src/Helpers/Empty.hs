{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.Empty (Empty(..)) where

import Data.Text (Text)
import Data.Time.Calendar (Day, toGregorian)

class Empty a where
    isEmpty :: a -> Bool

    isSet :: a -> Bool
    isSet = not . isEmpty

instance Empty Day where
    isEmpty day = year < 2
        where (year, _, _) = toGregorian day

instance Empty Text where
    isEmpty txt | txt == "" = True
                | txt == "000000" = True
                | otherwise = False