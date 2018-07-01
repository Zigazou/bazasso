{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Helpers.Like
    ( like
    , startsLike
    , endsLike
    , match
    ) where

import qualified Data.Text as T
import           Import

like :: EntityField record Text -> Text -> Filter record
like field val = Filter field
    (Left $ T.concat ["%", val, "%"])
    (BackendSpecificFilter "like")

startsLike :: EntityField record Text -> Text -> Filter record
startsLike field val = Filter field
    (Left $ T.concat [val, "%"])
    (BackendSpecificFilter "like")

endsLike :: EntityField record Text -> Text -> Filter record
endsLike field val = Filter field
    (Left $ T.concat ["%", val])
    (BackendSpecificFilter "like")

match :: EntityField record Text -> Text -> Filter record
match field val = Filter field (Left val) (BackendSpecificFilter "match")
