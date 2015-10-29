{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Test
    ( plugin
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import           System.Random

import Conduit

import           MonadBot.Types
import           MonadBot.Message

nigger :: SimpleHandler
nigger = handleBang "!nigger" $ do
    pref <- getPrefix
    case pref of
        Just (UserPrefix p _ _) -> do
            (chan:_) <- getParams
            sendCommand "PRIVMSG" [chan, p <> ": you are a nigger."]
        _ -> return ()


serverCmd :: SimpleHandler
serverCmd =
    handleBang "!servers" $ do
        srvs <- myServers <$> getGlobalEnv
        (chan:_) <- getParams
        sendCommand "PRIVMSG" [chan , "I am on the following servers and channels:"]
        forM_ srvs $ \srv ->
            sendCommand "PRIVMSG" [chan, serverAddress srv <> ": " <> T.intercalate ", " (serverChannels srv)]

test :: SimpleHandler
test =
    handleBang "!test" $ do
        d <- liftIO $ randomRIO (3, 10)
        (chan:_) <- getParams
        sendPrivmsg chan ["Waiting ", T.pack $ show d,  " seconds."]
        liftIO $ threadDelay $ d * 1000000
        sendPrivmsg chan ["Done."]

stateTest :: PluginM Int ()
stateTest = handleBang "!test" $ do
    (chan:_) <- getParams
    i <- readState
    sendPrivmsg chan ["Value before incrementing: ", T.pack $ show i]
    modifyState (+1)
    return ()

plugin :: Plugin Int
-- plugin = mkSimplePlugin "Test handler" [test, nigger, serverCmd]
plugin = Plugin "Test handler" [stateTest] c d
  where
    -- c :: Irc Int
    c = do
      -- logMsg "Running constructor"
      return 7
    -- d :: Int -> Irc ()
    d s = do
      -- logMsg "Running destructor"
      liftIO $ writeFile "test_handler.dat" (show s)
