{- |
Module      :  DateFormat
Description :  Format a date to DD/MM/YYYY format
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Format a date to DD/MM/YYYY format.
-}
module Helpers.DateFormat ( jjmmaaaa ) where

import           Import

-- | Format a date to DD/MM/YYYY format
jjmmaaaa :: Day    -- ^ The date to convert
         -> String -- ^ The result
jjmmaaaa day = formatTime defaultTimeLocale "%d/%m/%Y" t
    where t = UTCTime day 0 :: UTCTime
