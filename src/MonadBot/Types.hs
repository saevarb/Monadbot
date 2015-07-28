{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
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
import           MonadBot.Writer

-- |Monad for irc computations.
newtype Irc a
    = Irc
    { runIrc :: ReaderT Environment IO a
    } deriving
        ( Monad, MonadReader Environment
        , MonadIO, Functor)

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
    , plugName   :: Text
    }
