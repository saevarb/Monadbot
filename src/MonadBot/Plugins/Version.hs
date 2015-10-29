{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Version
    ( plugin
    ) where

import Paths_monadbot (version)
import Data.Version (showVersion)
import Data.Text (pack)

import MonadBot.Message
import MonadBot.Plugin.Development


versionHandler :: SimpleHandler
versionHandler = handlesCTCP "VERSION" $ do
    pref <- getPrefix
    case pref of
        Just (UserPrefix p _ _) ->
            ctcpReply p ["VERSION", "monadbot v" <> pack (showVersion version)]
        _ -> return ()

plugin :: Plugin ()
plugin = mkSimplePlugin "CTCP VERSION handler" [versionHandler]
