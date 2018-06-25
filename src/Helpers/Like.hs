{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.Like
    ( like
    , startsLike
    , endsLike
    ) where

import Import
import qualified Data.Text as T

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
    