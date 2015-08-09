{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Ping
    ( plugin
    ) where


import MonadBot.Types

pingHandler :: SimpleHandler
pingHandler _ = handles "PING" $ do
    params <- getParams
    sendCommand "PONG" params

plugin :: Plugin
plugin = mkSimplePlugin "Ping handler" [pingHandler]
