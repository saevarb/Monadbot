{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugin.Development
    ( getPluginName
    , getParams
    , getAuthEntries
    , modifyAuthEntries
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
    , whenOp
    , whenInGroup
    , handlesAny
    , handlesCTCP
    , ctcpReply
    , readState
    , putState
    , modifyState
    , swapState
    , readFileS
    , writeFileS
    , Prefix (..)
    , module MonadBot.Types
    ) where

import           Data.List
import qualified Data.Text as T
import           Data.Text.IO
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random
import           System.IO (withFile, IOMode (..))

import           MonadBot.Types
import           MonadBot.Message (Message (..), Prefix (..))

mkSimplePlugin :: Text -> [SimpleHandler] -> Plugin ()
mkSimplePlugin name hs =
    Plugin name hs (return ()) (const $ return ())

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
        ps <- getParams
        guard (not . null $ ps)
        let (_:cmd:_) = ps
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

getAuthEntries :: PluginM s AuthEntries
getAuthEntries = do
    (AuthInfo v) <- authInfo <$> getServerEnv
    liftIO . atomically $ readTMVar v

modifyAuthEntries :: (AuthEntries -> AuthEntries) -> PluginM s AuthEntries
modifyAuthEntries f = do
    (AuthInfo v) <- authInfo <$> getServerEnv
    ae <- getAuthEntries
    liftIO . atomically $ swapTMVar v $ f ae


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

readFileS :: FilePath -> Irc Text
readFileS f =
    liftIO $ withFile f ReadMode $ \h -> do
        contents <- hGetContents h
        T.length contents `seq` return contents

writeFileS :: FilePath -> Text -> Irc ()
writeFileS f c =
    liftIO $ withFile f WriteMode $ \h ->
        hPutStr h c

-- onlyForChannels :: [Text] -> PluginM a () -> PluginM a ()
whenOp :: PluginM s () -> PluginM s ()
whenOp f = handles "PRIVMSG" $ do
    (AuthEntries _ um) <- getAuthEntries
    (Just (pr@(UserPrefix n _ _))) <- getPrefix
    (chan:_) <- getParams
    if M.member pr um
    then f
    else do
      insult <- getRandomInsult n
      sendPrivmsg chan [insult]

getRandomInsult :: MonadIO m => Text -> m Text
getRandomInsult victim = do
    idx <- liftIO $ randomRIO (0, length insults - 1)
    return $ insults !! idx $ victim
  where
    insults =
        [ \n -> "That command is not for faggots, " <> n <> "."
        , \n -> "No, " <> n <> "."
        , const "Access denied."
        , (<> ": Nope.")
        ]

whenInGroup :: Text -> PluginM s () -> PluginM s ()
whenInGroup g f = handles "PRIVMSG" $ do
    (AuthEntries _ um) <- getAuthEntries
    (Just (pref@(UserPrefix n _ _))) <- getPrefix
    (chan:_) <- getParams
    case M.lookup pref um of
        (Just g') ->
            if g `S.member` g' then
              f
            else do
              insult <- getRandomInsult n
              sendPrivmsg chan [insult]
        Nothing -> do
            insult <- getRandomInsult n
            sendPrivmsg chan [insult]
