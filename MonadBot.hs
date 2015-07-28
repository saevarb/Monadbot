{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings          #-}
module MonadBot
    where

import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           MonadBot.Config
import           MonadBot.Logging
import           MonadBot.Message
import           MonadBot.Message.Encode
import           MonadBot.Message.Decode
import           MonadBot.Networking
import           MonadBot.Writer
import           MonadBot.ThreadManager
import           MonadBot.Types



makeEnv :: IrcConfig -> ServerInfo -> Logger -> Writer -> Environment
makeEnv cfg serv lgr w
    = Environment
    { myNick = nick cfg
    , server = serv
    , logger = lgr
    , writer = w
    }

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Create log and write queues
    logQueue    <- newTQueueIO
    writerQueue <- newTQueueIO

    -- Start logworker
    forkIO $ logWorker logQueue

    -- Make logger and writers
    let logger = makeLogger logQueue
        writer = makeWriter writerQueue


    forM_ (servers cfg) $ \s -> do
        let env = makeEnv cfg s logger writer
            cs  = clientSettings (serverPort s) u
            u   = TE.encodeUtf8 $ serverAddress s
        runTCPClient cs $ botApp env
