{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Join
    ( plugin
    ) where

import Control.Monad
import MonadBot.Plugin
import MonadBot.Types

joinHandler :: SimpleHandler
joinHandler _ = handles "376" $ do
    server <- getServer
    logMsg "Joining channels"
    forM_ (serverChannels server) $ \channel -> do
        sendCommand "JOIN" [channel]
        sendCommand "PRIVMSG" [channel, "fuck all you niggers"]

plugin = mkSimplePlugin "Join handler" [joinHandler]
