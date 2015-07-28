module MonadBot.Plugin
    where

import qualified Data.Text as T
import           Data.Text (Text)

import MonadBot
import MonadBot.Message
import MonadBot.Types

{- There are two types of 'plugins'.
-- 1. Persistent plugins. These plugins are launched in separate threads at
-- startup and subscribe to certain IRC messages(like PRIVMSG) and/or bang
-- commands, using channels to
-- receive these messages. They do not need initilization or
-- de-initialization as they are self-contained entities. If their threads
-- terminate unexpectedly(exceptions, etc) they will be restarted
-- automatically.
-- These kind of plugins are great for data-gathering stuff.
--
-- 2. One-time bang command handlers. These plugins run as soon as the bang
-- command they subscribe to is happens. They are run once in a sandboxed
-- thread(e.g. they will have a maximum lifetime) and they will be killed
-- once that time is over. They will have initialization and
-- deinitialization functions that will be run once upon startup and exit
-- respectively(for stuff like creating/deleting files and such.)
--
--}

data Subscription
    = BangCommand Text
    | IrcMessage  Text
    deriving (Show, Read, Eq)

data Plugin
    = PersistentPlugin
    { subscription :: [Subscription]
    -- TODO: Add comm channel
    , plugin       :: Irc ()
    , name         :: Text
    }
    | SandboxPlugin
    { constructor :: Irc ()
    , destructor  :: Irc ()
    , plugins     :: [(Subscription, Message -> Irc ())]
    , name        :: Text
    }

