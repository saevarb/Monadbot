{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugin.Ping
    ( plugin
    ) where


import MonadBot.Types

pingHandler :: SimpleHandler
pingHandler = handles "PING" $ do
    params <- getParams
    sendCommand "PONG" params

plugin :: Plugin ()
plugin = mkSimplePlugin "Ping handler" [pingHandler]
