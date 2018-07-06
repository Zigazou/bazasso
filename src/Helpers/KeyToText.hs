{- |
Module      :  KeyToText
Description :  Convert a Key to Text
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Convert a Key to Text.
-}
module Helpers.KeyToText ( keyToText ) where

import           Data.Either (fromRight)
import qualified Data.Text   as T
import           Import

keyToText :: PersistEntity record => Key record -> Text
keyToText = T.concat
          . fmap (fromRight ("" :: Text) . fromPersistValueText)
          . keyToValues
