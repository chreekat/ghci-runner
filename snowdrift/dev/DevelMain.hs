module Main where

import qualified Snowdrift
import Yesod

main = warp 3000 Snowdrift.Site
