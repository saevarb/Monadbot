{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Types
    where
import Conduit
import Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import Control.Monad.State
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T

import MonadBot.Logging
import MonadBot.Message
import MonadBot.MessageParser

class HasServerEnv s where
    getServerEnv :: (Monad m) => IrcT s m ServerEnvironment

instance HasServerEnv PluginEnvironment where
    getServerEnv = asks serverEnv

instance HasServerEnv ServerEnvironment where
    getServerEnv = ask

class HasGlobalEnv s where
    getGlobalEnv :: (Monad m) => IrcT s m GlobalEnvironment

instance HasGlobalEnv GlobalEnvironment where
    getGlobalEnv = ask

instance HasGlobalEnv ServerEnvironment where
    getGlobalEnv = asks globalEnv

instance HasGlobalEnv PluginEnvironment where
    getGlobalEnv = asks $ globalEnv . serverEnv

sendCommand :: (HasServerEnv s, MonadIO m) => Text -> [Text] -> IrcT s m ()
sendCommand c p = do
    w <- writer `fmap` getServerEnv
    liftIO . atomically . writeTQueue w $ Message Nothing c p

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

handleBang :: Text -> PluginM () -> PluginM ()
handleBang bang f =
    handles "PRIVMSG" $ do
        p <- getParams
        guard (not $ null p)
        let (_:first:_) = p
        when (T.tail first == bang) f

getServer :: PluginM ServerInfo
getServer = server `fmap` getServerEnv
