{-# LANGUAGE OverloadedStrings #-}
import           Data.Text
import           MonadBot.Config
import           MonadBot.Message
import           MonadBot.MessageParser

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Conduit
import           Data.Conduit.Network

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
    -- { serverPort     = 6667
    -- , serverAddress  = "irc.nixers.net"
    { serverPort     = 9999
    , serverAddress  = "127.0.0.1"
    , serverPass     = Nothing
    , serverChannels = ["#monadbot"]
    , nickServ       = Nothing
    }

foo :: Text
foo = "foo\nbar\nbaz"

settings :: ServerInfo -> ClientSettings
settings si =
    clientSettings (serverPort si) (TE.encodeUtf8 $ serverAddress si)

doStuff =
    awaitForever $ \line -> do
        let msg = decode line
        case msg of
          Left e -> liftIO $ putStrLn e
          Right m' -> yield m'

main :: IO ()
main =
    runTCPClient cs $ \app ->
        appSource app
        =$= decodeUtf8C
        =$= linesUnboundedC
        =$= mapC T.init
        =$= doStuff
        =$= printC
        =$= encodeUtf8C
        $$ appSink app

  where
    cs = settings testServer
