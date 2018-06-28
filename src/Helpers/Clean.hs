{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.Clean (clean) where

import Text.Blaze (ToMarkup, Markup)
import Helpers.Empty (Empty, isEmpty)
import Text.Hamlet (hamlet)

clean :: (ToMarkup a, Empty t) => (t -> a) -> t -> p -> Markup
clean after a = if isEmpty a
                    then [hamlet|<span .text-muted>non renseigné|]
                    else [hamlet|#{after a}|]