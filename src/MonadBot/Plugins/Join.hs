{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Join
    ( plugin
    ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import System.Random

import MonadBot.Types


messages :: [Text]
messages =
    [ "Carrier has arrived."
    , "Warp fields stabilized."
    , "It's a good day to die."
    , "We are as one."
    , "Adun Toridas."
    , "I have returned."
    , "Teleport successful."
    , "My life for Aiur!"
    , "Monadbot operational."
    , "Can I take your order?"
    , "Somebody call for an exterminator?"
    , "Monadbot online."
    , "SCV good to go, sir."
    ]

joinChannel channel = do
    choice <- liftIO $ randomRIO (0, m)
    sendCommand "JOIN" [channel]
    sendPrivmsg channel [messages !! choice]
  where
    m = length messages - 1

joinHandler :: SimpleHandler
joinHandler _ = handles "376" $ do
    srv <- getServer
    logMsg "Joining channels"
    mapM_ joinChannel (serverChannels srv)

rejoin :: SimpleHandler
rejoin _ = handleBang "!rejoin" $ do
    (chan:_) <- getParams
    sendCommand "PART" [chan]
    joinChannel chan

plugin :: Plugin
plugin = mkSimplePlugin "Join handler" [joinHandler, rejoin]
