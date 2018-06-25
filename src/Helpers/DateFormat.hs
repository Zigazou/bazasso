{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.DateFormat
    ( jjmmaaaa
    ) where

import Import

jjmmaaaa :: Day -> String
jjmmaaaa day = formatTime defaultTimeLocale "%d/%m/%Y" t
    where t = UTCTime day 0 :: UTCTime
