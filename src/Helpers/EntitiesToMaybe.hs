{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  EntitiesToMaybe
Description :  Converting specific List to Maybe
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Converting specific List to Maybe.
-}
module Helpers.EntitiesToMaybe
    ( entitiesToMaybe
    , singlesToMaybe
    ) where

import           Import

import           Database.Persist.Sql (Single (..))

-- | Converts a list of `Entity` to a `Maybe`. It takes the first item of the
--   list (if present) and returns it in a `Just` value. It returns `Nothing`
--   otherwise.
entitiesToMaybe :: [Entity a] -> Maybe a
entitiesToMaybe (Entity _ x:_) = Just x
entitiesToMaybe _              = Nothing

-- | Converts a list of `Single` to a `Maybe`. It takes the first item of the
--   list (if present) and returns it in a `Just` value. It returns `Nothing`
--   otherwise.
singlesToMaybe :: [Single a] -> Maybe a
singlesToMaybe (Single x:_) = Just x
singlesToMaybe _            = Nothing
