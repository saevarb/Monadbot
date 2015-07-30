{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Types
    where
import Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T

import           MonadBot.Config
import           MonadBot.Logging
import MonadBot.Message
import MonadBot.MessageParser

-- | The Irc Environment. The parts of the environment that are modifiable
-- are stored in TMVars. One of these is created for every server
-- connection.
data Environment
    = Environment
    { server      :: ServerInfo
    , myNick      :: Text
    , writer      :: Writer
    , logger      :: Logger
    , threadStore :: TMVar ThreadStore
    , threadLife  :: TMVar Int
    }

instance Show Environment where
    show (Environment { .. }) =
        "Environment " ++ T.unpack myNick ++ " " ++ show server

-- | Monad for irc computations.
newtype IrcT m a
    = IrcT
    { unIrc :: ReaderT Environment m a
    } deriving
        ( Alternative, Applicative, Monad, MonadReader Environment
        , MonadIO, Functor)

instance MonadTrans IrcT where
    lift = IrcT . lift

type Irc = IrcT IO

runIrc :: Irc a -> Environment -> IO a
runIrc irc = runReaderT (unIrc irc)

-- TODO: Move elsewhere. Utilities?
sendCommand :: (MonadIO m) => Text -> [Text] -> IrcT m ()
sendCommand c p = do
    w <- asks writer
    liftIO . w $ Message Nothing c p

logMsg :: (MonadIO m) => Text -> IrcT m ()
logMsg msg = do
    lgr <- asks logger
    liftIO . lgr $ msg


getThreadLife :: Irc Int
getThreadLife = do
    tlvar <- threadLife <$> ask
    liftIO . atomically $ readTMVar tlvar

setThreadLife :: Int -> Irc ()
setThreadLife tl = do
    tlvar <- threadLife <$> ask
    liftIO . atomically $ swapTMVar tlvar tl
    return ()

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

type Writer = Message -> IO ()

makeWriter :: TQueue Message -> Writer
makeWriter q m =
    liftIO . atomically $ writeTQueue q m
