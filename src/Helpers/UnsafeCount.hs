{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.UnsafeCount
    ( unsafeCount
    ) where

import           Import

import qualified Data.Text            as T
import           Database.Persist.Sql (Single, rawSql)

unsafeCount :: DBparam Text [Single Int]
unsafeCount tableName = rawSql queryCount []
    where
        queryCount = T.concat [ "SELECT MAX(_ROWID_) "
                              , "FROM ", tableName, " "
                              , "LIMIT 1;"
                              ]
