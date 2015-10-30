import MonadBot

testConfig :: IrcConfig
testConfig = cfg { timeout = 5 }
  where
    cfg = makeIrcConfig "monadbot"
        "monadbot"
        "monadbot"
        [darchoods]

nixers :: ServerInfo
nixers =
    makeServerInfo "irc.nixers.net"
        6667
        ["#monadbot", "#nixers"]
        False

darchoods :: ServerInfo
darchoods =
    addOp "sbrg" "~sbrg" "reid-87n.7ls.62.178.IP" "owner" $
    makeServerInfo "127.0.0.1"
        6667
        ["#420", "#bots"]
        False

main :: IO ()
main = runBot testConfig
