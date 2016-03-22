{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Join
    ( plugin
    ) where
import System.Random
import qualified Data.Text as T
import MonadBot.Plugin.Development


messages :: [Text]
messages =
    [ "Carrier has arrived."
    , "Warp fields stabilized."
    , "It's a good day to die."
    , "We are as one."
    , "Adun Toridas."
    , "En Taro Adun."
    , "I have returned."
    , "Teleport successful."
    , "My life for Aiur!"
    , "Monadbot operational."
    , "Can I take your order?"
    , "Somebody call for an exterminator?"
    , "Monadbot online."
    , "SCV good to go, sir."
    ]

joinChannel :: (MonadIO m, HasServerEnv s) => Text -> IrcT s m ()
joinChannel channel = do
    choice <- liftIO $ randomRIO (0, m)
    sendCommand "JOIN" [channel]
    sendPrivmsg channel [messages !! choice]
  where
    m = length messages - 1

joinHandler :: SimpleHandler
joinHandler = onCmd "376" $ do
    srv <- getServer
    logMsg "Joining channels"
    mapM_ joinChannel (serverChannels srv)

rejoin :: SimpleHandler
rejoin = onUserCmd "$rejoin" $ do
    (chan:_) <- getParams
    sendCommand "PART" [chan]
    joinChannel chan

joinCmd :: SimpleHandler
joinCmd = onUserCmd "$join" $ do
   (_:_:chan:_) <- getParams
   joinChannel chan

kickHandler :: SimpleHandler
kickHandler = onCmd "KICK" $ do
    (channel:target:_) <- getParams
    me <- getMyNick
    when (target == me) $ joinChannel channel

plugin :: Plugin ()
plugin = mkSimplePlugin "Join handler" [kickHandler, joinCmd, joinHandler, rejoin]
