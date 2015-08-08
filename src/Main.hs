{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent hiding (yield)
import           MonadBot.Types
import           MonadBot.Message
import           MonadBot.MessageParser
import           MonadBot.Logging
import           MonadBot.Config
import           MonadBot.Writer
import           MonadBot.Plugin
import           MonadBot.Plugins

import           Data.Maybe
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TQueue
import           Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Conduit
import           Data.Conduit.Network
import           Text.Printf

testConfig :: IrcConfig
testConfig =
    IrcConfig
    { nick    = "monadbot"
    , user    = "bar"
    , real    = "baz"
    , servers = [nixers, darchoods]
    }

nixers :: ServerInfo
nixers
    = ServerInfo
    { serverPort     = 6667
    , serverAddress  = "irc.nixers.net"
    , serverPass     = Nothing
    , serverChannels = ["#monadbot"]
    , nickServ       = Nothing
    }

darchoods :: ServerInfo
darchoods
    = ServerInfo
    { serverPort     = 6697
    , serverAddress  = "irc.darchoods.net"
    , serverPass     = Nothing
    , serverChannels = ["#420"]
    , nickServ       = Nothing
    }

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

handleMessage :: [InitializedPlugin] -> Consumer Text (Sink () ServerM) ()
handleMessage plugins =
    awaitForever $ \line -> do
        let msg = decode line
        case msg of
          Left e ->
              liftIO $ putStrLn e
          Right m' -> do
              lift . lift $ logMsg (ppMessage m')
              -- lgr <- lift . lift $  logger `fmap` getGlobalEnv
              -- liftIO . atomically $ writeTQueue lgr $ ppMessage m'
              forM_ plugins $ \p@(InitializedPlugin n hs) -> do
                  sEnv <- lift . lift $ getServerEnv
                  let pEnv = PluginEnvironment sEnv m' p
                  forM_ hs $ \handler -> liftIO $ runReaderT (unIrc handler) pEnv
                  return ()


botApp :: ServerEnvironment -> AppData -> IO ()
botApp srvEnv app =
    runConduit . runReaderC srvEnv $ hoist unIrc $ do
        -- Start writer thread
        wq <- asks writer
        liftIO . forkIO . forever $ do
            msg <- atomically $ readTQueue wq
            yield (encode msg) =$= encodeUtf8C $$ appSink app

        gEnv <- lift getGlobalEnv
        initPlugins <- liftIO $ mapM (initalizePlugin gEnv) allPlugins

        -- Send authentication messages
        lift authenticate

        -- Decode bytestrings to Text
        appSource app =$= decodeUtf8C
            -- Chunk into lines
            =$= linesUnboundedC
            -- Remove trailing \r
            =$= mapC T.init
            -- Handle messages
            $$ handleMessage initPlugins
  where
    authenticate :: ServerM ()
    authenticate = do
        authMsgs <- mkAuthMessage
        yieldMany authMsgs
            =$= mapC encode
            =$= encodeUtf8C
            $$ appSink app

    -- mkAuthMessage :: Producer ServerM Message
    mkAuthMessage :: ServerM [Message]
    mkAuthMessage = do
        sp <- (serverPass . server) `fmap` getServerEnv
        n  <- myNick `fmap` getGlobalEnv
        return $ catMaybes
            [ maybe Nothing (\p -> Just $ Message Nothing "PASS" [p]) sp
            , return $ Message Nothing "NICK" [n]
            , return $ Message Nothing "USER" [n <> " 0 *", "monadbot"]
            ]

makeGlobalEnv :: IrcConfig -> IO GlobalEnvironment
makeGlobalEnv cfg = do
    -- Create log queue which is shared by all servers
    lgr <- newTQueueIO
    return $ GlobalEnvironment (nick cfg) (servers cfg) lgr -- undefined undefined

makeServerEnv :: GlobalEnvironment -> ServerInfo -> IO ServerEnvironment
makeServerEnv gEnv srv = do
    -- Writer specific to server
    w <- newTQueueIO
    return $ ServerEnvironment gEnv srv w

connectToServer :: ServerEnvironment -> IO ()
connectToServer srvEnv = do
        let cs = settings (server srvEnv)
        -- TODO: This should be forkIO'd
        void . forkIO $ runTCPClient cs (botApp srvEnv)

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Start logworker
    gEnv <- makeGlobalEnv cfg
    putStrLn "Starting logger.."
    forkIO $ logWorker (logger gEnv)

    forM_ (servers cfg) $ \srv -> do
        printf "Connecting to %s..\n" (T.unpack $ serverAddress srv)
        srvEnv <- makeServerEnv gEnv srv
        connectToServer srvEnv
    forever $ threadDelay 10000

main :: IO ()
main = runBot testConfig
