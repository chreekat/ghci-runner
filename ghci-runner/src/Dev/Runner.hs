module Dev.Runner (update, shutdown) where

import Control.Exception (finally)
import Control.Monad ((>=>), forever)
import Control.Concurrent
import Data.IORef
import Foreign.Store
import GHC.Word

-- | (Re)start the computation.
update :: IO () -> IO () -> IO ()
update comp abrt = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no computation running
      Nothing -> do
          lock <- storeAction lockStore newEmptyMVar
          tid <- start lock
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- computation is already running
      Just tidStore -> restartAppInNewThread tidStore
  where
    lockStore :: Store (MVar ())
    lockStore = Store 0

    -- shut the computation down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore lockStore takeMVar
        readStore lockStore >>= start


    -- | Start the computation in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start lock = do
        forkIO (finally comp
                        -- Note that this implies concurrency
                        -- between shutdownApp and the next app that is starting.
                        -- Normally this should be fine
                        (putMVar lock () >> abrt))

-- | kill the computation
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no computation running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "computation shut down"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
