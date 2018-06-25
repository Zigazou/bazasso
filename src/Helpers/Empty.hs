{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.Empty
    ( Empty(..)
    , clean
    ) where

import Import
import Text.Blaze (ToMarkup, Markup)

class Empty a where
    isEmpty :: a -> Bool

instance Empty Day where
    isEmpty day = year < 2
        where (year, _, _) = toGregorian day

instance Empty Text where
    isEmpty txt | txt == "" = True
                | txt == "000000" = True
                | otherwise = False

clean :: (ToMarkup a, Empty t) => (t -> a) -> t -> p -> Markup
clean after a = if isEmpty a
                    then [hamlet|<span .text-muted>non renseigné|]
                    else [hamlet|#{after a}|]
