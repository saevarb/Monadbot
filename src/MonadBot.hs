{-# LANGUAGE RankNTypes #-}
module MonadBot
    ( runBot
    , IrcConfig (..)
    , ServerInfo (..)
    ) where

import           Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue)
import           Control.Monad
import           Control.Monad.Morph (hoist)
import           Control.Monad.Reader (asks)
import           Control.Monad.STM (atomically)
import           Data.Maybe (catMaybes)
import           Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Exit
import           System.Posix.Signals

import           Conduit
import           Data.Conduit.Network
import           Text.Printf

import           MonadBot.Types
import           MonadBot.Message
import           MonadBot.Message.Encode
import           MonadBot.Message.Decode
import           MonadBot.Logging
import           MonadBot.Plugins
import           MonadBot.Sandbox (sandbox)
import           MonadBot.Utility (ppMessage)

handleMessage :: (HasServerEnv s, MonadIO m)
              => [Hide InitializedPlugin]
              -> Consumer Text (Sink () (IrcT s m)) ()
handleMessage plugins =
    awaitForever $ \line -> do
        let msg = decode line
        case msg of
          Left e ->
              liftIO $ putStrLn e
          Right m' -> do
              lift . lift $ logMsg (ppMessage m')
              forM_ plugins $ \(Hide p@(InitializedPlugin _ s hs)) -> do
                  sEnv <- lift . lift $ getServerEnv
                  let pEnv = PluginEnvironment sEnv m' p s
                  mapM_ (sandbox pEnv) hs


botApp :: ServerEnvironment -> AppData -> IO ()
botApp srvEnv app =
    runConduit . runReaderC srvEnv $ hoist unIrc $ do
        -- Start writer thread
        wq <- asks writer
        liftIO . forkIO . forever $ do
            msg <- atomically $ readTQueue wq
            yield (encode msg) =$= encodeUtf8C $$ appSink app
            threadDelay 250000

        gEnv <- lift getGlobalEnv
        initPlugins <- liftIO $ mapM (initializePlugin gEnv) allPlugins

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
    tlife <- newTMVarIO $ timeout cfg * 1000000
    lgr <- newTQueueIO
    return $ GlobalEnvironment (nick cfg) (servers cfg) lgr tlife

makeServerEnv :: GlobalEnvironment -> ServerInfo -> IO ServerEnvironment
makeServerEnv gEnv srv = do
    -- Writer specific to server
    w <- newTQueueIO
    return $ ServerEnvironment gEnv srv w

connectToServer :: ServerEnvironment -> IO ThreadId
connectToServer srvEnv = do
        let cs = settings (server srvEnv)
        -- TODO: This should be forkIO'd
        forkIO $ runTCPClient cs (botApp srvEnv)
  where
    settings :: ServerInfo -> ClientSettings
    settings si =
        clientSettings (serverPort si) (TE.encodeUtf8 $ serverAddress si)

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Start logworker
    gEnv <- makeGlobalEnv cfg
    putStrLn "Starting logger.."
    forkIO $ logWorker (logger gEnv)

    threads <- forM (servers cfg) $ \srv -> do
        printf "Connecting to %s..\n" (T.unpack $ serverAddress srv)
        srvEnv <- makeServerEnv gEnv srv
        connectToServer srvEnv

    installHandler sigINT (sigIntHandler threads) Nothing

    getChar
    putStrLn "Killing all threads"
    mapM_ killThread threads
  where
    sigIntHandler threads =
        CatchOnce $
            putStrLn "Killing all threads" >>
            mapM_ killThread threads >>
            exitSuccess
