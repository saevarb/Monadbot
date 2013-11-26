{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module MonadBot
    where

import Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Data.Conduit
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import           Data.Conduit.Network
import           Data.Conduit.Text
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text              (Text)

import           MonadBot.Config
import           MonadBot.Logging
import           MonadBot.Message
import           MonadBot.Networking

newtype Irc a
    = Irc
    { runIrc :: StateT Environment IO a
    } deriving
        ( Monad, MonadState Environment
        , MonadIO, Functor)

data Environment
    = Environment
    { server    :: ServerInfo
    , myNick    :: Text
    , ircWriter :: Message -> IO ()
    , logger    :: Logger
    }

instance Show Environment where
    show (Environment s n _ _ ) = 
        "Environment " ++ T.unpack n ++ " " ++ show s

data Context
    = Context
    { message     :: Message
    , environment :: Environment
    }

mkAuthMessage :: Environment -> [Message]
mkAuthMessage env = catMaybes
    [ (serverPass . server $ env) >>= \p -> return $ Message Nothing "PASS" [p]
    , return $ Message Nothing "NICK" [myNick env]
    , return $ Message Nothing "USER" [myNick env, "0", "*", ":fake"]
    ]

doStuff :: Environment -> Conduit Text IO Text
doStuff env = undefined
     
botApp :: Environment -> Application IO
botApp env appData = do
    appSource appData
    $= CB.lines
    $= decode utf8
    $= doStuff env
    $= encode utf8
    $$ appSink appData 

initalize :: Source IO Text
initalize = undefined

makeEnv :: IrcConfig -> ServerInfo -> Logger -> Environment
makeEnv cfg serv lgr
    = Environment
    { myNick   = nick cfg
    , server   = serv
    , logger   = lgr
    }

runBot :: IrcConfig -> IO ()
runBot cfg = do
    logQueue <- newTQueueIO
    forkIO $ logWorker logQueue
    let logger = makeLogger logQueue
    forM_ (servers cfg) $ \s -> do
        let env = makeEnv cfg s logger
        print env


