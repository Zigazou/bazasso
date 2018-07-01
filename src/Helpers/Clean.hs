{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Clean (clean) where

import           Helpers.Empty (Empty, isEmpty)
import           Text.Blaze    (Markup, ToMarkup)
import           Text.Hamlet   (hamlet)

clean :: (ToMarkup a, Empty t) => (t -> a) -> t -> p -> Markup
clean after a = if isEmpty a then [hamlet|<span .text-muted>non renseigné|]
                             else [hamlet|#{after a}|]
