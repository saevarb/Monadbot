{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Types
    where
import qualified Data.Text.IO as TIO
import           Conduit
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           MonadBot.Logging
import           MonadBot.Message
import           MonadBot.Message.Decode
import           MonadBot.Message.Encode

class HasServerEnv s where
    getServerEnv :: (Monad m) => IrcT s m ServerEnvironment

instance HasServerEnv PluginEnvironment where
    getServerEnv = asks serverEnv

instance HasServerEnv ServerEnvironment where
    getServerEnv = ask

class HasGlobalEnv s where
    getGlobalEnv :: (Monad m) => IrcT s m GlobalEnvironment

instance (HasServerEnv a) => HasGlobalEnv a where
    getGlobalEnv = globalEnv <$> getServerEnv

-- | The bot's config.
data IrcConfig =
    IrcConfig
    { nick    :: Text
    , user    :: Text
    , real    :: Text
    , servers :: [ServerInfo]
    } deriving (Eq, Read, Show)

-- | Contains information needed to connect to a server.
data ServerInfo
    = ServerInfo
    { serverPort     :: Int
    , serverAddress  :: Text
    , serverPass     :: Maybe Text
    , serverChannels :: [Text]
    , useTLS         :: Bool
    , nickServ       :: Maybe (Text, Text)
    } deriving (Eq, Read, Show)


-- | The global environment. This contains all resources that are shared between servers.
-- The data that can be modified is stored in 'TMVars' such that any modifications will
-- be visible everywhere.
data GlobalEnvironment
    = GlobalEnvironment
    { myNick      :: Text
    , myServers   :: [ServerInfo]
    , logger      :: Logger
    -- , threadStore :: TMVar ThreadStore
    -- , threadLife  :: TMVar Int
    }

-- | A server environment. Contains state that is accessible in the context of a specific
-- server, plus the global state.
data ServerEnvironment
    = ServerEnvironment
    { globalEnv :: GlobalEnvironment
    , server    :: ServerInfo
    , writer    :: Writer
    }

-- | A plugin environment. Contains state that is accessible in the context of a specific
-- rnning plugin, plus the server state and global state.
data PluginEnvironment
    = PluginEnvironment
    { serverEnv :: ServerEnvironment
    , message   :: Message
    , handler   :: InitializedPlugin
    }

-- | Monad for irc computations.
newtype IrcT s m a
    = IrcT
    { unIrc :: ReaderT s m a
    } deriving
        ( MonadThrow, Alternative, Applicative, Monad, MonadReader s
        , MonadIO, Functor)

type Irc                 = IrcT GlobalEnvironment IO
type ServerM             = IrcT ServerEnvironment IO
type PersistentPluginM a = IrcT ServerEnvironment (StateT a IO)
type PluginM             = IrcT PluginEnvironment IO

data Subscription

type SimpleHandler = () -> PluginM ()

data Plugin
   = forall a. Plugin
   { plugName :: Text
   , handlers :: [a -> PluginM ()]
   , initialize :: Irc a
   }
   | forall a. PersistentPlugin
   { construct :: Irc a
   , destruct  :: a -> Irc ()
   , daemons :: [PersistentPluginM a ()]
   }

data InitializedPlugin
    = InitializedPlugin Text [PluginM ()]

logMsg :: (MonadIO m, HasGlobalEnv s) => Text -> IrcT s m ()
logMsg msg = do
    lgr <- logger `fmap` getGlobalEnv
    liftIO . atomically . writeTQueue lgr $ msg

runIrc :: Irc a -> GlobalEnvironment -> IO a
runIrc irc = runReaderT (unIrc irc)

-- getThreadLife :: Irc Int
-- getThreadLife = do
--     tlvar <- threadLife <$> ask
--     liftIO . atomically $ readTMVar tlvar

-- setThreadLife :: Int -> Irc ()
-- setThreadLife tl = do
--     tlvar <- threadLife <$> ask
--     liftIO . atomically $ swapTMVar tlvar tl
--     return ()

--------------------
-- Threading
--------------------

type ThreadStore = S.Set Thread

data ThreadType
    = Sandboxed
    | NotSandboxed
    deriving (Show, Eq)

data Thread
    = Thread
    { handle     :: Async ()
    , threadType :: ThreadType
    , startTime  :: Int
    -- , plugName   :: Plugin
    }

type Writer = TQueue Message

------------------------------------------------------------------
-- Plugin stuff
------------------------------------------------------------------
initalizePlugin :: GlobalEnvironment -> Plugin -> IO InitializedPlugin
initalizePlugin _ (PersistentPlugin {..}) = error "initalizePlugin: not defined"
initalizePlugin env (Plugin {..}) = do
    x <- runIrc initialize env
    return $ InitializedPlugin plugName (map ($ x) handlers)

mkSimplePlugin :: Text -> [SimpleHandler] -> Plugin
mkSimplePlugin name handlers =
    Plugin name handlers (return ())

getMessage :: PluginM Message
getMessage = asks message

getPrefix :: PluginM (Maybe Prefix)
getPrefix = prefix `fmap` getMessage

getParams :: PluginM [Text]
getParams = params `fmap` getMessage

getCommand :: PluginM Text
getCommand = command `fmap` getMessage

getPlugin :: PluginM InitializedPlugin
getPlugin = asks handler

handles :: Text -> PluginM () -> PluginM ()
handles c f = do
    cmd <- getCommand
    when (cmd == c) f

handlesAny :: [Text] -> PluginM () -> PluginM ()
handlesAny cmds f =
    mapM_ (flip handles f) cmds

handlesCTCP :: Text -> PluginM () -> PluginM ()
handlesCTCP c f =
    handlesAny ["PRIVMSG", "NOTICE"] $ do
        params <- getParams
        guard (not . null $ params)
        let (_:cmd:_) = params
        when ("\x01" <> c <> "\x01" == T.tail cmd) f


handleBang :: Text -> PluginM () -> PluginM ()
handleBang bang f =
    handles "PRIVMSG" $ do
        p <- getParams
        guard (not $ null p)
        let (_:first:_) = p
        when (T.tail first == bang) f

onlyForServer :: Text -> PluginM () -> PluginM ()
onlyForServer srv f = do
    addr <- serverAddress <$> getServer
    when (addr == srv) f

onlyForChannel :: Text -> PluginM () -> PluginM ()
onlyForChannel channel f = do
    (chan:_) <- getParams
    when (chan == channel) f

getServer :: PluginM ServerInfo
getServer = server `fmap` getServerEnv

sendCommand :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
sendCommand c p = do
    w <- writer `fmap` getServerEnv
    logMsg $ "Sending command: " <> (encode $ Message Nothing c p)
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

runPlugin :: MonadIO m => PluginEnvironment -> PluginM a -> m a
runPlugin pEnv h =  liftIO $ runReaderT (unIrc h) pEnv
