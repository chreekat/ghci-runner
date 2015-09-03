module DevelRunner (main, shutdown) where

import qualified Snowdrift
import Yesod
import qualified Dev.Runner as Dev

main = Dev.update (warp 3000 Snowdrift.Site) (return ())
shutdown = Dev.shutdown
