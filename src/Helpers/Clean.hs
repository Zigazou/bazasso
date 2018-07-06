{- |
Module      :  Clean
Description :
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Generates and handles a search form requiring a city and themes to be selected.
-}
module Helpers.Clean (clean) where

import           Import.NoFoundation

import           Helpers.Empty       (Empty, isEmpty)
import           Text.Blaze          (ToMarkup)

{- |
    Creates a renderer displaying 'non renseigné' if the value to display is
    empty, or a filtered value if the value is not empty.
    If no filter is required, the `id` function can be given.
-}
clean :: (ToMarkup output, Empty input)
      => (input -> output) -- ^ A function to apply to the value before display
      -> input             -- ^ The value to display
      -> Html              -- ^ The Html
clean after a = if isEmpty a then [shamlet|<span .text-muted>non renseigné|]
                             else [shamlet|#{after a}|]
