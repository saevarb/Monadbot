{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugin.Development
    ( getPluginName
    , getParams
    , sendPrivmsg
    , handles
    , handleBang
    , sendCommand
    , mkSimplePlugin
    , getServer
    , getPrefix
    , onlyForServer
    , onlyForChannel
    , onlyForChannels
    , handlesAny
    , handlesCTCP
    , ctcpReply
    , readState
    , putState
    , modifyState
    , swapState
    , Prefix (..)
    , module MonadBot.Types
    ) where

import Data.List
import qualified Data.Text as T

import MonadBot.Types
import MonadBot.Message (Message (..), Prefix (..))
import MonadBot.Message.Encode (encode)

mkSimplePlugin :: Text -> [SimpleHandler] -> Plugin ()
mkSimplePlugin name handlers =
    Plugin name handlers (return ()) (const $ return ())

getMessage :: PluginM a Message
getMessage = asks message

getPrefix :: PluginM a (Maybe Prefix)
getPrefix = prefix `fmap` getMessage

getParams :: PluginM a [Text]
getParams = params `fmap` getMessage

getCommand :: PluginM a Text
getCommand = command `fmap` getMessage

getPlugin :: PluginM a (InitializedPlugin a)
getPlugin = asks handler

handles :: Text -> PluginM a () -> PluginM a ()
handles c f = do
    cmd <- getCommand
    when (cmd == c) f

handlesAny :: [Text] -> PluginM a () -> PluginM a ()
handlesAny cmds f =
    mapM_ (`handles` f) cmds

handlesCTCP :: Text -> PluginM a () -> PluginM a ()
handlesCTCP c f =
    handlesAny ["PRIVMSG", "NOTICE"] $ do
        params <- getParams
        guard (not . null $ params)
        let (_:cmd:_) = params
        when ("\x01" <> c <> "\x01" == T.tail cmd) f


handleBang :: Text -> PluginM a () -> PluginM a ()
handleBang bang f =
    handles "PRIVMSG" $ do
        p <- getParams
        guard (not $ null p)
        let (_:first:_) = p
        when (T.tail first == bang) f

onlyForServer :: Text -> PluginM a () -> PluginM a ()
onlyForServer srv f = do
    addr <- serverAddress <$> getServer
    when (addr == srv) f

onlyForChannel :: Text -> PluginM a () -> PluginM a ()
onlyForChannel channel f = do
    (chan:_) <- getParams
    when (chan == channel) f

onlyForChannels :: [Text] -> PluginM a () -> PluginM a ()
onlyForChannels channels f = do
    (chan:_) <- getParams
    maybe (return ()) (const f) $ find (== chan) channels

getServer :: PluginM a ServerInfo
getServer = server `fmap` getServerEnv

sendCommand :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
sendCommand c p = do
    w <- writer `fmap` getServerEnv
    logMsg $ "Sending command: " <> encode (Message Nothing c p)
    liftIO . atomically . writeTQueue w $ Message Nothing c p

sendPrivmsg :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
sendPrivmsg target msg = sendCommand "PRIVMSG" $ target : msg

sendNotice :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
sendNotice target msg = sendCommand "NOTICE" $ target : msg

ctcpCommand :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
ctcpCommand target = sendPrivmsg target . ctcpify

ctcpReply :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
ctcpReply target = sendNotice target . ctcpify

ctcpify :: [Text] -> [Text]
ctcpify (x:xs) = ("\x01" <> x) : init xs <> [last xs <> "\x01"]
ctcpify [] = error "ctcpify: This shouldn't happen. Please report a bug."

getPluginName :: PluginM s Text
getPluginName = do
    (InitializedPlugin name _ _ _) <- getPlugin
    return name

readState :: PluginM s s
readState = do
    (PluginState v) <- asks state
    liftIO . atomically $ readTMVar v
    -- return s

putState :: s -> PluginM s ()
putState s = do
   (PluginState v) <- asks state
   liftIO . atomically $ putTMVar v s

swapState :: s -> PluginM s s
swapState s = do
   (PluginState v) <- asks state
   liftIO . atomically $ swapTMVar v s

modifyState :: (s -> s) -> PluginM s s
modifyState f = readState >>= swapState . f

-- withState :: (s -> PluginM s ()) -> PluginM s ()
-- withState f = do
--     get
--     liftIO . atomically $ putTMVar v (f s)
