{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Plugin
    where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T

import           MonadBot.Message
import           MonadBot.Types

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

data PluginState = PluginState
    { message :: Message
    , handler :: Handler
    }

-- | Monad for plugins.
newtype PluginM m a
    = PluginM
    { unPlugin :: ReaderT Environment m a
    } deriving
        ( Alternative, Applicative, Monad, MonadReader Environment
        , MonadIO, Functor)

type Plugin = PluginM (ReaderT PluginState IO)
type PersistentPlugin a = PluginM (StateT a IO)

type SimpleHandler = () -> Plugin ()

instance MonadTrans PluginM where
    lift = PluginM . lift

data Handler
   = forall a. Handler
   { plugName :: Text
   , handlers :: [a -> Plugin ()]
   , initialize :: Irc a
   }

mkSimplePlugin :: Text -> [SimpleHandler] -> Handler
mkSimplePlugin name handlers =
    Handler name handlers (return ())

getMessage :: Plugin Message
getMessage = lift $ asks message

getPrefix :: Plugin (Maybe Prefix)
getPrefix = prefix `fmap` getMessage

getParams :: Plugin [Text]
getParams = params `fmap` getMessage

getCommand :: Plugin Text
getCommand = command `fmap` getMessage

getHandler :: Plugin Handler
getHandler = lift $ asks handler

handles :: Text -> Plugin () -> Plugin ()
handles c f = do
    cmd <- getCommand
    when (cmd == c) f

handleBang :: Text -> Plugin () -> Plugin ()
handleBang bang f =
    handles "PRIVMSG" $ do
        p <- getParams
        guard (not $ null p)
        when (head p == bang) f
