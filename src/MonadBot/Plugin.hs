{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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

data Subscription

type PluginM = IrcT (ReaderT PluginState IO)
type PersistentPluginM a = IrcT (StateT a IO)

type SimpleHandler = () -> PluginM ()

data PluginState = PluginState
    { message :: Message
    , handler :: Plugin
    }

data Plugin
   = forall a. Plugin
   { plugName :: Text
   , handlers :: [a -> PluginM ()]
   , initialize :: Irc a
   }
   | forall a. PersistentPlugin
   { construct :: Irc a
   , destruct  :: a -> Irc ()
   , daemons :: [PersistentPluginM a ()]
   }

data InitializedPlugin
    = InitializedPlugin Text [PluginM ()]

initalizePlugin :: Plugin -> Environment -> IO InitializedPlugin
initalizePlugin (Plugin {..}) env = do
    x <- runIrc initialize env
    return $ InitializedPlugin plugName (map ($ x) handlers)

mkSimplePlugin :: Text -> [SimpleHandler] -> Plugin
mkSimplePlugin name handlers =
    Plugin name handlers (return ())

getMessage :: PluginM Message
getMessage = lift $ asks message

getPrefix :: PluginM (Maybe Prefix)
getPrefix = prefix `fmap` getMessage

getParams :: PluginM [Text]
getParams = params `fmap` getMessage

getCommand :: PluginM Text
getCommand = command `fmap` getMessage

getPlugin :: PluginM Plugin
getPlugin = lift $ asks handler

handles :: Text -> PluginM () -> PluginM ()
handles c f = do
    cmd <- getCommand
    when (cmd == c) f

handleBang :: Text -> PluginM () -> PluginM ()
handleBang bang f =
    handles "PRIVMSG" $ do
        p <- getParams
        guard (not $ null p)
        when (head p == bang) f
