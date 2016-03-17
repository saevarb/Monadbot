{-# LANGUAGE TemplateHaskell #-}
module MonadBot.Config
       ( readConfig
       , IrcConfig (..)
       , ServerInfo (..)
       ) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Yaml

data IrcConfig =
    IrcConfig
    { nick        :: Text
    , user        :: Text
    , real        :: Text
    , servers     :: [ServerInfo]
    , timeout     :: Int
    } deriving (Eq, Read, Show)

data ServerInfo
    = ServerInfo
    { serverPort     :: Int
    , serverAddress  :: Text
    , serverPass     :: Maybe Text
    , serverChannels :: [Text]
    , useTLS         :: Bool
    , nickServ       :: Maybe (Text, Text)
    } deriving (Eq, Read, Show)

$(deriveJSON defaultOptions ''ServerInfo)
$(deriveJSON defaultOptions ''IrcConfig)

readConfig :: FilePath -> IO IrcConfig
readConfig path = do
    res <- decodeFileEither path
    case res of
        Left ex -> error $ "Error while parsing config: " ++ prettyPrintParseException ex
        Right cfg -> return cfg
