{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Sandbox
    where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Conduit
import Control.Concurrent.Async

import MonadBot.Types
import MonadBot.ThreadManager

data ThreadState
    = Done
    | Running
    | Exception SomeException
    deriving (Show)

sandbox :: MonadIO m => PluginEnvironment -> PluginM a -> m ()
sandbox pEnv f =
    void . liftIO . async $ runPlugin pEnv $ do
        delay <- getThreadLife
        name <- getPluginName
        t <- liftIO $ async $ runPlugin pEnv f
        liftIO $ threadDelay delay
        res <- liftIO $ poll t
        case res of
            Nothing -> do
                liftIO $ cancel t
                (chan:_) <- getParams
                logMsg $ "Plugin '" <> name <> "' exceeded time limit and was killed."
                sendPrivmsg chan ["Plugin '" <> name <> "' exceeded time limit and was killed."]
            Just res' ->
                case res' of
                    Left (SomeException e) ->
                        logMsg $ "Plugin '" <> name <> "' failed with exception " <> (T.pack . show $ e)
                    Right _ -> return ()
                        -- logMsg $ "Plugin '" <> name <> "' ran successfully."
