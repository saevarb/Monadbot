{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Join
    ( plugin
    ) where

import Control.Monad

import MonadBot.Types

joinHandler :: SimpleHandler
joinHandler _ = handles "376" $ do
    srv <- getServer
    logMsg "Joining channels"
    forM_ (serverChannels srv) $ \channel ->
        sendCommand "JOIN" [channel]

plugin :: Plugin
plugin = mkSimplePlugin "Join handler" [joinHandler]
