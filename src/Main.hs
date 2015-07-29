{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Reader
import           Control.Monad.STM
import           MonadBot.Types
import           MonadBot.Message
import           MonadBot.MessageParser
import           MonadBot.Logging
import           MonadBot.Config
import           MonadBot.Writer

import           Control.Monad
import           Data.Maybe
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TQueue
import           Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Conduit
import           Data.Conduit.Network
import Text.Printf

testConfig :: IrcConfig
testConfig =
    IrcConfig
    { nick    = "monadbot"
    , user    = "bar"
    , real    = "baz"
    , servers = [testServer]
    }

testServer :: ServerInfo
testServer
    = ServerInfo
    { serverPort     = 6667
    , serverAddress  = "irc.nixers.net"
    , serverPass     = Nothing
    , serverChannels = ["#monadbot"]
    , nickServ       = Nothing
    }

makeEnv :: IrcConfig -> ServerInfo -> Logger -> Writer -> Environment
makeEnv cfg serv lgr w
    = Environment
    { myNick = nick cfg
    , server = serv
    , logger = lgr
    , writer = w
    }


mkAuthMessage :: Environment -> [Message]
mkAuthMessage env = catMaybes
    [ (serverPass . server $ env) >>= \p -> return $ Message Nothing "PASS" [p]
    , return $ Message Nothing "NICK" [myNick env]
    , return $ Message Nothing "USER" [myNick env, "0", "*", ":monadbot"]
    ]

settings :: ServerInfo -> ClientSettings
settings si =
    clientSettings (serverPort si) (TE.encodeUtf8 $ serverAddress si)

ppMessage :: Message -> Text
ppMessage msg =
    case prefix msg of
      (Just (ServerPrefix p)) ->
          T.pack $ printf "[%s] %s %s" (T.unpack p) (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)
      (Just (UserPrefix p _ _)) ->
          T.pack $ printf "[%s] %s %s" (T.unpack p) (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)
      Nothing ->
          T.pack $ printf "%s %s" (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)

-- handleMessage :: Consumer Text IO ()
handleMessage :: Consumer Text (ReaderT Environment IO) ()
handleMessage =
    awaitForever $ \line -> do
        let msg = decode line
        case msg of
          Left e -> liftIO $ putStrLn e
          Right m' -> do
              lgr <- asks logger
              liftIO . lgr $ ppMessage m'

botApp :: TQueue Message -> Environment -> AppData -> IO ()
botApp wq env app = do
    -- Start writer thread
    forkIO $ do
        msg <- atomically $ readTQueue wq
        yield (encode msg) =$= encodeUtf8C $$ appSink app

    -- Send authentication messages
    yieldMany (mkAuthMessage env) =$= mapC encode =$= encodeUtf8C $$ appSink app

    -- Decode bytestrings to Text
    appSource app =$= decodeUtf8C
        -- Chunk into lines
        =$= linesUnboundedC
        -- Remove trailing \r
        =$= mapC T.init
        -- Handle messages
        $$ runReaderC env handleMessage

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Create log queue which is shared by all servers
    logQueue    <- newTQueueIO
    -- Make logger
    let logger = makeLogger logQueue
    -- Start logworker
    putStrLn "Starting logger.."
    forkIO $ logWorker logQueue

    forM_ (servers cfg) $ \srv -> do
        printf "Connecting to %s..\n" (T.unpack $ serverAddress srv)
        writerQueue <- newTQueueIO
        let writer = makeWriter writerQueue
            env    = makeEnv cfg srv logger writer
            cs     = clientSettings (serverPort srv) (TE.encodeUtf8 $ serverAddress srv)
        -- This should be forkIO'd
        runTCPClient cs $ botApp writerQueue env

main :: IO ()
main = runBot testConfig
