import MonadBot

testConfig :: IrcConfig
testConfig =
    IrcConfig
    { nick    = "monadbot"
    , user    = "bar"
    , real    = "baz"
    , servers = [darchoods]
    , timeout = 10
    }

nixers :: ServerInfo
nixers
    = ServerInfo
    { serverPort     = 6667
    , serverAddress  = "irc.nixers.net"
    , serverPass     = Nothing
    , useTLS         = False
    , serverChannels = ["#monadbot"]
    , nickServ       = Nothing
    }

darchoods :: ServerInfo
darchoods
    = ServerInfo
    { serverPort     = 6667
    , serverAddress  = "127.0.0.1"
    , serverPass     = Nothing
    , serverChannels = ["#bots"]
    , useTLS         = False
    , nickServ       = Nothing
    }

main :: IO ()
main = runBot testConfig
