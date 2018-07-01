{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module      :  Like
Description :  Like operators for Persistent queries
Copyright   :  (c) Frédéric BISSON
License     :  GPL-2
Maintainer  :  zigazou@free.fr

Like operators for Persistent queries
-}
module Helpers.Like
    ( like
    , startsLike
    , endsLike
    , match
    ) where

import qualified Data.Text as T
import           Import

-- | Implements the `like` operator. It automatically add '%' before and after
--   the string to compare to.
like :: EntityField record Text -- ^ Field to filter on
     -> Text                    -- ^ Text to compare with
     -> Filter record           -- ^ Resulting filter
like field val = Filter field
                        (Left $ T.concat ["%", val, "%"])
                        (BackendSpecificFilter "like")

-- | Implements the `startsLike` operator. It automatically add '%' after the
--   string to compare to.
startsLike :: EntityField record Text -- ^ Field to filter on
           -> Text                    -- ^ Text to compare with
           -> Filter record           -- ^ Resulting filter
startsLike field val = Filter field
                              (Left $ T.concat [val, "%"])
                              (BackendSpecificFilter "like")

-- | Implements the `endsLike` operator. It automatically add '%' before the
--   string to compare to.
endsLike :: EntityField record Text  -- ^ Field to filter on
         -> Text                     -- ^ Text to compare with
         -> Filter record            -- ^ Resulting filter
endsLike field val = Filter field
                            (Left $ T.concat ["%", val])
                            (BackendSpecificFilter "like")

-- | Implements the `match` operator. This operator is specific to Sqlite3 and
--   is used to look for keywords in FTS3/4/5 tables.
match :: EntityField record Text    -- ^ Field to filter on
      -> Text                       -- ^ Text to compare with
      -> Filter record              -- ^ Resulting filter
match field val = Filter field (Left val) (BackendSpecificFilter "match")
