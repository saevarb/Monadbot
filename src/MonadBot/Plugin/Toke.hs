{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugin.Toke
    ( plugin
    ) where

import Control.Monad
import qualified Data.Text as T

import MonadBot.Plugin
import MonadBot.Types
import MonadBot.Message

toke :: SimpleHandler
toke _ =
    onlyForServer "127.0.0.1"
    onlyForChannel "#420" $
    handleBang "!toke" $ do
        mapM_ (sendPrivmsg "#420")
        sendCommand "PRIVMSG" [chan, serverAddress server <> ": " <> T.intercalate ", " (serverChannels server)]


plugin :: Plugin
plugin = mkSimplePlugin "Toke handler" [toke]
