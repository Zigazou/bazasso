{- |
Module      :  Empty
Description :  Class to handle empty content without the use of Maybe
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Class to handle empty content without the use of Maybe. It allows to read
content from a database where nullable values were not correctly handled.
-}
module Helpers.Empty ( Empty(..) ) where

import           ClassyPrelude.Yesod
import           Data.Text          (Text)
import           Data.Time.Calendar (Day, toGregorian)

-- | The Empty class is there to handle empty content without the use of Maybe.
--   It's a workaround to work with databases where nullable values
--   cannot/were not correctly handled.
class Empty a where
    -- | Tells if a value is empty. It must be defined for each type
    --   implementing the Empty class.
    isEmpty :: a -> Bool

    -- | Tells if a value is set. It needs not be defined for each type.
    isSet :: a -> Bool
    isSet = not . isEmpty

-- | Instance for the `Day` type.
instance Empty Day where
    isEmpty day = year < 2
        where (year, _, _) = toGregorian day

-- | Instance for the `Text` type.
instance Empty Text where
    isEmpty txt | txt == "" = True
                | txt == "000000" = True
                | otherwise = False
