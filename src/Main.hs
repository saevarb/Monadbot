{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
import           MonadBot.Types
import           MonadBot.Message
import           MonadBot.MessageParser
import           MonadBot.Logging
import           MonadBot.Config
import           MonadBot.Writer
import MonadBot.Plugin
import MonadBot.Plugins

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

handleMessage :: Consumer Text (Sink () ServerM) ()
handleMessage =
    awaitForever $ \line -> do
        let msg = decode line
        case msg of
          Left e ->
              liftIO $ putStrLn e
          Right m' -> do
              lgr <- (lift . lift $  logger `fmap` getGlobalEnv)
              liftIO . atomically $ writeTQueue lgr $ ppMessage m'
              return ()

botApp :: ServerEnvironment -> AppData -> IO ()
botApp srvEnv app =
    runConduit . runReaderC srvEnv $ hoist unIrc $ do
        -- Start writer thread
        wq <- asks writer
        liftIO . forkIO . forever $ do
            msg <- atomically $ readTQueue wq
            yield (encode msg) =$= encodeUtf8C $$ appSink app

        -- Send authentication messages
        lift authenticate

        env <- lift getServerEnv
        -- Decode bytestrings to Text
        appSource app =$= decodeUtf8C
            -- Chunk into lines
            =$= linesUnboundedC
            -- Remove trailing \r
            =$= mapC T.init
            -- Handle messages
            $$ handleMessage
  where
    -- authenticate :: Producer ServerM ()
    authenticate =
        mkAuthMessage
        =$= mapC encode
        =$= encodeUtf8C
        $$ appSink app

    mkAuthMessage :: Producer ServerM Message
    mkAuthMessage = do
        sp <- (serverPass . server) `fmap` lift getServerEnv
        n  <- myNick `fmap` lift getGlobalEnv
        yieldMany $ catMaybes
            [ maybe Nothing (\p -> Just $ Message Nothing "PASS" [p]) sp
            , return $ Message Nothing "NICK" [n]
            , return $ Message Nothing "USER" [n, "0", "*", ":monadbot"]
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
        runTCPClient cs (botApp srvEnv)

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Start logworker
    gEnv <- makeGlobalEnv cfg
    putStrLn "Starting logger.."
    forkIO $ logWorker (logger gEnv)

    -- initPlugins <- forM plugins $ \p ->
    forM_ (servers cfg) $ \srv -> do
        printf "Connecting to %s..\n" (T.unpack $ serverAddress srv)
        srvEnv <- makeServerEnv gEnv srv
        connectToServer srvEnv

main :: IO ()
main = runBot testConfig
