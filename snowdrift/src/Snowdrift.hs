{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Snowdrift (Site(..)) where

import Yesod

data Site = Site

mkYesod "Site" [parseRoutes|
/ HomeR GET
|]

instance Yesod Site

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello world!|]
