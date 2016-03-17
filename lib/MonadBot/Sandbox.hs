{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Sandbox
    where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Text as T

import Conduit
import Control.Concurrent.Async

import MonadBot.Types
import MonadBot.ThreadManager
import MonadBot.Plugin.Development

data ThreadState
    = Done
    | Running
    | Exception SomeException
    deriving (Show)

sandbox :: MonadIO m => PluginEnvironment s -> PluginM s a -> m ()
sandbox pEnv f =
    void . liftIO . async . runPlugin pEnv $ do
        delay <- getThreadLife
        name <- getPluginName
        t <- liftIO . async $ runPlugin pEnv f
        liftIO $ threadDelay delay
        res <- liftIO $ poll t
        (chan:_) <- getParams
        case res of
            Nothing -> do
                liftIO $ cancel t
                let msg = "Plugin '" <> name <> "' exceeded time limit and was killed."
                logMsg msg
                sendPrivmsg chan [msg]
            Just res' ->
                case res' of
                    Left (SomeException e) -> do
                        let msg = "Plugin '" <> name <> "' failed with exception: " <> (T.pack . show $ e)
                        logMsg msg
                        sendPrivmsg chan [msg]
                    Right _ -> return ()
                        -- logMsg $ "Plugin '" <> name <> "' ran successfully."
