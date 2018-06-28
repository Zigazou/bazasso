{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Helpers.DateFormat
    ( jjmmaaaa
    ) where

import Import

jjmmaaaa :: Day -> String
jjmmaaaa day = formatTime defaultTimeLocale "%d/%m/%Y" t
    where t = UTCTime day 0 :: UTCTime
