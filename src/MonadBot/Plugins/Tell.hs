{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Tell
    ( plugin
    ) where


import qualified Data.Text as T
import qualified Data.Map.Strict as M

import MonadBot.Plugin.Development

data TellState
    = TellState
    { messages :: M.Map (Text, Text) (Text, Text)
    } deriving (Show, Read)


tellCmd :: PluginM TellState ()
tellCmd = onUserCmd "$tell" $ do
    (Just (UserPrefix sender _ _)) <- getPrefix
    (channel:_:person:msg) <- getParams
    sendPrivmsg channel ["Your message to", person, "has been noted."]
    modifyState $ \s ->
        s { messages = M.insert (channel, person) (sender, T.unwords msg) (messages s) }
    return ()

tellTrigger :: PluginM TellState ()
tellTrigger = onCmd "PRIVMSG" $ do
    (Just (UserPrefix sender _ _)) <- getPrefix
    (channel:_) <- getParams
    (TellState tm) <- readState
    case M.lookup (channel, sender) tm of
        Just (from, msg) ->
            sendPrivmsg channel ["Message from", from <> ":", msg]
        Nothing -> return ()

plugin :: Plugin TellState
plugin = Plugin "Tell functionality"
         [tellCmd, tellTrigger]
         (return $ TellState M.empty)
         (const $ return ())
