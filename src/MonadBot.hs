{-# LANGUAGE RankNTypes #-}
module MonadBot
    ( runBot
    , IrcConfig (..)
    , defaultConfig
    , ServerInfo (..)
    , defaultServerInfo
    , addOp
    ) where

import           Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Morph (hoist)
import           Control.Monad.Reader (asks)
import           Control.Monad.STM (atomically)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           System.Directory
import           System.Exit
import           System.Posix.Signals

import           Conduit
import           Data.Conduit.Network
import           Data.Conduit.Network.TLS
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
              forM_ plugins $ \(Enabled p@(InitializedPlugin _ s hs _)) -> do
                  sEnv <- lift . lift $ getServerEnv
                  let pEnv = PluginEnvironment sEnv m' p s
                  mapM_ (sandbox pEnv) hs


botApp :: ServerEnvironment -> AppData -> IO ()
botApp srvEnv app = do
    let gEnv = globalEnv srvEnv
    initPlugins <- liftIO $ mapM (initializePlugin gEnv)
                   [Enabled p | Enabled p <- allPlugins]
    handle (\ThreadKilled -> runIrc (callDestructors initPlugins) gEnv) $
        runConduit . runReaderC srvEnv $ hoist unIrc $ do
            -- Start writer thread
            wq <- asks writer
            liftIO . forkIO . forever $ do
                msg <- atomically $ readTQueue wq
                yield (encode msg) =$= encodeUtf8C $$ appSink app
                threadDelay 250000

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

makeServerEnv :: Text -> GlobalEnvironment -> ServerInfo -> IO ServerEnvironment
makeServerEnv filename gEnv srv = do
    -- Writer specific to server
    w <- newTQueueIO
    ai <- AuthInfo <$> (readAuthEntries >>= newTMVarIO)
    return $ ServerEnvironment gEnv srv w ai
  where
    readAuthEntries :: IO AuthEntries
    -- readAuthEntries = do
    readAuthEntries = do
        fileOk <- doesFileExist $ T.unpack filename
        if fileOk
        then read . T.unpack <$> TIO.readFile (T.unpack filename)
        else return (authEntries srv)

connectToServer :: ServerEnvironment -> IO ThreadId
connectToServer srvEnv =
    case useTLS (server srvEnv) of
        False ->
            forkIO $ runTCPClient (nonTLSSettings $ server srvEnv) (botApp srvEnv)
        _ ->
            forkIO $ runTLSClient (tlsSettings $ server srvEnv) (botApp srvEnv)
  where
    nonTLSSettings :: ServerInfo -> ClientSettings
    nonTLSSettings si =
        clientSettings (serverPort si) (TE.encodeUtf8 $ serverAddress si)
    tlsSettings :: ServerInfo -> TLSClientConfig
    tlsSettings si =
        tlsClientConfig (serverPort si) (TE.encodeUtf8 $ serverAddress si)

runBot :: IrcConfig -> IO ()
runBot cfg = do
    -- Start logworker
    gEnv <- makeGlobalEnv cfg
    putStrLn "Starting logger.."
    forkIO $ logWorker (logger gEnv)

    dirOk <- doesDirectoryExist (T.unpack $ authInfoDir cfg)
    unless dirOk $
        createDirectory (T.unpack $ authInfoDir cfg)


    srvEnvs <- mapM (\s -> makeServerEnv (mkAuthFile s) gEnv s) (servers cfg)
    threads <- forM srvEnvs $ \se -> do
        printf "Connecting to %s..\n" (T.unpack $ serverAddress . server $ se)
        connectToServer se

    installHandler sigINT (sigIntHandler threads) Nothing

    getChar
    putStrLn "Killing all threads"
    mapM_ killThread threads
    putStrLn "Saving auth info.."
    forM_ srvEnvs $ \se -> do
        let (AuthInfo v) = authInfo se
        ae <- liftIO . atomically $ readTMVar v
        TIO.putStrLn $ "\t" <> serverAddress (server se)
        TIO.writeFile (T.unpack . mkAuthFile $ server se) $ T.pack $ show ae
    putStrLn "Goodbye."
  where
    sigIntHandler threads =
        CatchOnce $
            putStrLn "Killing all threads" >>
            mapM_ killThread threads >>
            exitSuccess
    mkAuthFile s =
        (T.reverse . T.dropWhile (== '/') . T.reverse $ authInfoDir cfg)
        <> "/" <> serverAddress s <> ".auth"

callDestructors :: [Hide InitializedPlugin] -> IrcT GlobalEnvironment IO ()
callDestructors plugins =
    forM_ plugins $ \(Enabled p) -> do
        let (PluginState v) = _state p
        s <- liftIO . atomically $ readTMVar v
        _destructor p s
