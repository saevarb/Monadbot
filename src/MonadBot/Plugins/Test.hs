{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Test
    ( plugin
    ) where

import Control.Monad
import qualified Data.Text as T

import MonadBot.Plugin
import MonadBot.Types
import MonadBot.Message

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


plugin :: Plugin
plugin = mkSimplePlugin "Test handler" [nigger, serverCmd]
