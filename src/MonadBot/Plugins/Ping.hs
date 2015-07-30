{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Ping
    ( plugin
    ) where

import MonadBot.Plugin
import MonadBot.Types


pingHandler :: SimpleHandler
pingHandler _ = handles "PING" $ do
    params <- getParams
    logMsg "Sent pong"
    sendCommand "PONG" params

plugin = mkSimplePlugin "Ping pingHandler" [pingHandler]

-- plugin :: Plugin
-- plugin = Plugin "Ping pingHandler" [pingHandler] noInit
-- Alternatively
-- plugin = mkSimplePlugin "Ping pingHandler" [pingHandler]
