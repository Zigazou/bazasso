{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.EntitiesToMaybe
    ( entitiesToMaybe
    , singlesToMaybe
    ) where

import           Import

import           Database.Persist.Sql (Single (..))

entitiesToMaybe :: [Entity a] -> Maybe a
entitiesToMaybe [Entity _ x] = Just x
entitiesToMaybe _            = Nothing

singlesToMaybe :: [Single a] -> Maybe a
singlesToMaybe [Single x] = Just x
singlesToMaybe _          = Nothing
