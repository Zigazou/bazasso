{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.Quasi

import           Data.ActivityMode
import           Data.ActivityPosition
import           Data.EmailAddr
import           Data.Ess
import           Data.Group
import           Data.Nature
import           Data.PhoneNumber
import           Data.Productive
import           Data.Rup
import           Data.SalariedEmployees
import           Data.SeasonalActivity
import           Data.Siret
import           Data.WebSite

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "config/models")
