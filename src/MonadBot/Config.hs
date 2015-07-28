module MonadBot.Config where

import Data.Text  (Text)
import Data.Maybe (Maybe(..))

data IrcConfig =
    IrcConfig
    { nick    :: Text
    , user    :: Text
    , real    :: Text
    , servers :: [ServerInfo]
    } deriving (Eq, Read, Show)


data ServerInfo
    = ServerInfo
    { serverPort     :: Int
    , serverAddress  :: Text
    , serverPass     :: Maybe Text
    , serverChannels :: [Text]
    , nickServ       :: Maybe (Text, Text)
    } deriving (Eq, Read, Show)

