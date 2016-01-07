{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Ping
    ( plugin
    ) where

import MonadBot.Plugin.Development

pingHandler :: SimpleHandler
pingHandler = onCmd "PING" $ do
    params <- getParams
    sendCommand "PONG" params

plugin :: Plugin ()
plugin = mkSimplePlugin "Ping handler" [pingHandler]
