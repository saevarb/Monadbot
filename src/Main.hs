import MonadBot

testConfig :: IrcConfig
testConfig = cfg { timeout = 5 }
  where
    cfg = makeIrcConfig "monadbot-test"
        "monadbot-test"
        "monadbot-test"
        [darchoods]

darchoods :: ServerInfo
darchoods =
    makeServerInfo "irc.darkscience.net"
        6697
        ["#bots"]
        True

main :: IO ()
main = runBot testConfig
