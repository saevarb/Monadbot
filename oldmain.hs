{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import           MonadBot
import           MonadBot.Config
import           MonadBot.Message
import           MonadBot.MessageParser

testConfig :: IrcConfig
testConfig =
    IrcConfig
    { nick    = "foo"
    , user    = "bar"
    , real    = "baz"
    , servers = [testServer]
    }

testServer :: ServerInfo
testServer
    = ServerInfo
    { serverPort     = 6667
    , serverAddress  = "wulfware.uk.tddirc.net"
    , serverPass     = Nothing
    , serverChannels = ["#monadbot"]
    , nickServ       = Nothing
    }


main :: IO [Text]
main = undefined
