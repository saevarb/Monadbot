import MonadBot

testConfig :: IrcConfig
testConfig =
    defaultConfig
    { nick = "MonadBot-test"
    , servers = [darchoods]
    }

darchoods :: ServerInfo
darchoods =
    defaultServerInfo
    { serverAddress = "irc.darkscience.net"
    , serverChannels = ["#bots"]
    }

main :: IO ()
main = runBot testConfig
