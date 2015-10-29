{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Types
    ( liftIO
    , Text
    , HasGlobalEnv (..)
    , HasServerEnv (..)
    , IrcConfig (..)
    , IrcT (..)
    , Hide (..)
    , Irc
    , PluginM
    , ServerM
    , SimpleHandler
    , ServerInfo (..)
    , GlobalEnvironment (..)
    , ServerEnvironment (..)
    , PluginEnvironment (..)
    , Plugin (..)
    , InitializedPlugin (..)
    , PluginState (..)
    , initializePlugin
    , runPlugin
    , getPluginName
    , getParams
    , logMsg
    , sendPrivmsg
    , handles
    , handleBang
    , sendCommand
    , mkSimplePlugin
    , getServer
    , getPrefix
    , onlyForServer
    , onlyForChannel
    , handlesAny
    , handlesCTCP
    , ctcpReply
    , readTMVar
    , atomically
    , readState
    , putState
    , modifyState
    , swapState
    , (<>)
    , runIrc
    , Prefix (..)
    ) where


import           Conduit
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State hiding (state)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           MonadBot.Logging
import           MonadBot.Message

import           MonadBot.Message.Encode

class HasServerEnv s where
    getServerEnv :: (Monad m) => IrcT s m ServerEnvironment

instance HasServerEnv (PluginEnvironment a) where
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
    , timeout :: Int
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
    , threadLife  :: TMVar Int
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
data PluginEnvironment a
    = PluginEnvironment
    { serverEnv :: ServerEnvironment
    , message   :: Message
    , handler   :: InitializedPlugin a
    , state     :: PluginState a
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
type PluginM s           = IrcT (PluginEnvironment s) IO

data Hide f = forall a. Hide (f a)
type SimpleHandler = PluginM () ()

data Plugin s
   = Plugin
   { pluginName  :: Text
   , handlers    :: [PluginM s ()]
   , constructor :: Irc s
   , destructor  :: s -> Irc ()
   }
   -- | forall a. PersistentPlugin
   -- { construct :: Irc a -- , :: a -> Irc ()
   -- , daemons :: [PersistentPluginM a ()]
   -- }

data InitializedPlugin s
    = InitializedPlugin
    { _pluginName :: Text
    , _state      :: PluginState s
    , _handlers   :: [PluginM s ()]
    , _destructor  :: s -> Irc ()
    }

data PluginState a = PluginState (TMVar a)

mkPluginState :: a -> IO (PluginState a)
mkPluginState a = do
    var <- newTMVarIO a
    return $ PluginState var

runIrc :: Irc a -> GlobalEnvironment -> IO a
runIrc irc = runReaderT (unIrc irc)

type Writer = TQueue Message

------------------------------------------------------------------
-- Plugin stuff
------------------------------------------------------------------
initializePlugin :: GlobalEnvironment -> Hide Plugin -> IO (Hide InitializedPlugin)
-- initializePlugin _ (PersistentPlugin {..}) =
--     error "initializePlugin (PersistentPlugin (..)): not defined"
initializePlugin env (Hide (Plugin {..})) = do
    x <- runIrc constructor env
    s <- mkPluginState x
    return $ Hide $ InitializedPlugin pluginName s handlers destructor

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

logMsg :: (MonadIO m, HasGlobalEnv s) => Text -> IrcT s m ()
logMsg msg = do
    lgr <- logger `fmap` getGlobalEnv
    liftIO . atomically . writeTQueue lgr $ msg

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

runPlugin :: MonadIO m => PluginEnvironment s -> PluginM s a -> m a
runPlugin pEnv h =  liftIO $ runReaderT (unIrc h) pEnv

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
