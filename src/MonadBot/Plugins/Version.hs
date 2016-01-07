{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Version
    ( plugin
    ) where

import Paths_monadbot (version)
import Data.Version (showVersion)
import Data.Text (pack)
import Data.Time (getCurrentTime)


import MonadBot.Message
import MonadBot.Plugin.Development


versionHandler :: SimpleHandler
versionHandler = onCtcp "VERSION" $ do
    pref <- getPrefix
    case pref of
        Just (UserPrefix p _ _) ->
            ctcpReply p ["VERSION", "monadbot v" <> pack (showVersion version)]
        _ -> return ()

userVersion :: SimpleHandler
userVersion = onUserCmds ["$version", "$info"] $ do
    return ()

initState :: PluginM UTCTime UTCTime
initState =
    liftIO getCurrentTime

plugin :: Plugin ()
plugin =
    Plugin
    "Version and info handler"
    [versionHandler]
    initState
    (const $ return ())
