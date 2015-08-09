{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Test
    ( plugin
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import           System.Random

import Conduit

import           MonadBot.Types
import           MonadBot.Message

nigger :: SimpleHandler
nigger _ = handleBang "!nigger" $ do
    pref <- getPrefix
    case pref of
        Just (UserPrefix p _ _) -> do
            (chan:_) <- getParams
            sendCommand "PRIVMSG" [chan, p <> ": you are a nigger."]
        _ -> return ()


serverCmd :: SimpleHandler
serverCmd _ =
    handleBang "!servers" $ do
        srvs <- myServers <$> getGlobalEnv
        (chan:_) <- getParams
        sendCommand "PRIVMSG" [chan , "I am on the following servers and channels:"]
        forM_ srvs $ \srv ->
            sendCommand "PRIVMSG" [chan, serverAddress srv <> ": " <> T.intercalate ", " (serverChannels srv)]

test :: SimpleHandler
test _ =
    handleBang "!test" $ do
        d <- liftIO $ randomRIO (3, 10)
        (chan:_) <- getParams
        sendPrivmsg chan ["Waiting ", T.pack $ show d,  " seconds."]
        liftIO $ threadDelay $ d * 1000000
        sendPrivmsg chan ["Done."]

plugin :: Plugin
plugin = mkSimplePlugin "Test handler" [test, nigger, serverCmd]
