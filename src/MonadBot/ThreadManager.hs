module MonadBot.ThreadManager
    where

import qualified Data.Set as S
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader

import MonadBot.Types

{-
--
-- The thread manager will manage threads, killing off threads that run too
-- long(bang commands) and restarting threads that die(persistent plugins).
-- This means that the 'thread store' needs to distinguish between
-- persistent threads and sandboxed threads.
-}

-- getThreadStore :: Irc ThreadStore
-- getThreadStore = do
--     tsvar <- reader threadStore
--     liftIO . atomically $ takeTMVar tsvar

-- putThreadStore :: ThreadStore -> Irc ()
-- putThreadStore ts = do
--     tsvar <- reader threadStore
--     liftIO . atomically $ putTMVar tsvar ts

-- withThreadStore :: (ThreadStore -> Irc ThreadStore) -> Irc ()
-- withThreadStore f = do
--     ts <- getThreadStore
--     ts' <- f ts
--     putThreadStore ts'

-- threadManager :: Irc ()
-- threadManager = withThreadStore $ \ts -> do
--     undefined

getThreadLife :: (MonadIO m, HasGlobalEnv s) => IrcT s m Int
getThreadLife = do
    tlvar <- threadLife <$> getGlobalEnv
    liftIO . atomically $ readTMVar tlvar

setThreadLife :: Int -> Irc ()
setThreadLife tl = do
    tlvar <- threadLife <$> ask
    liftIO . atomically $ swapTMVar tlvar tl
    return ()

--------------------
-- Threading
--------------------

type ThreadStore = S.Set Thread

data Thread
    = Thread
    { handle    :: Async ()
    , startTime :: Int
    , plugin    :: InitializedPlugin
    }
