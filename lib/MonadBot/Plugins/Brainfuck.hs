{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Brainfuck
    ( plugin
    ) where

import qualified Data.Text as T

import MonadBot.Plugin.Development
import MonadBot.Plugins.Brainfunk

plugin :: Plugin ()
plugin = mkSimplePlugin "Brainfuck plugin" [brainfuckHandler]

-- rejoin :: SimpleHandler
-- rejoin = handleBang "$rejoin" $ do
--     (chan:_) <- getParams
--     sendCommand "PART" [chan]
--     joinChannel chan

brainfuckHandler :: SimpleHandler
brainfuckHandler = onUserCmd "$brainfuck" $ do
    (chan:msg) <- getParams
    result <- liftIO . runProgram . T.unpack $ T.concat msg
    case result of
      Left e -> sendPrivmsg chan ["Error occurred: ", T.pack e]
      Right e -> sendPrivmsg chan ["Output: ", T.pack e]
