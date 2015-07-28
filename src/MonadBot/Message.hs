module MonadBot.Message
    ( Message (..)
    , Prefix  (..)
    , (<>)
    )
    where

import Data.Text (Text)
import Data.Monoid ((<>))


data Message
    = Message
    { prefix  :: Maybe Prefix
    , command :: Text
    , params  :: [Text]
    } deriving (Show, Eq, Read)


data Prefix
    = ServerPrefix Text
    | UserPrefix Text (Maybe Text) (Maybe Text)
    deriving (Show, Eq, Read)




