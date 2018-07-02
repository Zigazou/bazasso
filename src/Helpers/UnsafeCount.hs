{- |
Module      :  UnsafeCount
Description :  Unsafe count of entries in a Sqlite3 table
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Unsafe count of entries in a Sqlite3 table.
-}
module Helpers.UnsafeCount ( unsafeCount ) where

import           Import

import qualified Data.Text            as T
import           Database.Persist.Sql (Single, rawSql)

{- |
Create a query to get the number of rows in an Sqlite3 table. This unsafe count
will work only on tables which had no rows deleted! It returns in fact the last
rowid created. It allows to instantly get the number of rows of an Sqlite3
table. A `SELECT COUNT` always has to read the complete table to get that
information.
--}
unsafeCount :: DBparam Text [Single Int]
unsafeCount tableName = rawSql queryCount []
    where
        queryCount = T.concat [ "SELECT MAX(_ROWID_) "
                              , "FROM ", tableName, " "
                              , "LIMIT 1;"
                              ]
