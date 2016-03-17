{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Version
    ( plugin
    ) where

import Data.List
import Data.Text (pack)
import Data.Version (showVersion)
import Paths_monadbot (version)
import Data.Time
import Data.Time.Clock

import MonadBot.Message
import MonadBot.Plugin.Development


versionHandler :: PluginM UTCTime ()
versionHandler = onCtcp "VERSION" $ do
    pref <- getPrefix
    case pref of
        Just (UserPrefix p _ _) ->
            ctcpReply p ["VERSION", "monadbot v" <> pack (showVersion version)]
        _ -> return ()

userVersion :: PluginM UTCTime ()
userVersion = onUserCmds ["$version", "$info"] $ do
    (channel:_) <- getParams
    sendPrivmsg channel ["I am monadbot v" <> pack (showVersion version)]
    start <- readState
    current <- liftIO getCurrentTime
    let diff = round $ diffUTCTime current start
    let formatted = formatSeconds diff
    sendPrivmsg channel ["Uptime:", pack formatted]

formatSeconds :: Integer -> String
formatSeconds s = unwords $ formatSeconds' s units
  where
   formatSeconds' _ [] = []
   formatSeconds' remainder ((label, time):us) =
       case divMod remainder time of
           (0, r) ->
               formatSeconds' r us
           (n, r) ->
               (show n ++ label) : formatSeconds' r us
   units =
       [ ("y", 365 * 24 * 60 * 60)
       , ("m", 30 * 24 * 60 * 60)
       , ("d", 24 * 60 * 60)
       , ("h", 60 * 60)
       , ("m", 60)
       , ("s", 1)
       ]

initState :: Irc UTCTime
initState =
    liftIO getCurrentTime

plugin :: Plugin UTCTime
plugin =
    Plugin
    "Version and info handler"
    [versionHandler, userVersion]
    initState
    (const $ return ())
