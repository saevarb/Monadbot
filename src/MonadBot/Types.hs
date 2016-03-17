{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Types
    ( HasGlobalEnv (..)
    , HasServerEnv (..)
    , Hide (..)
    , IrcT (..)
    , Irc
    , PluginM
    , ServerM
    , SimpleHandler
    , AuthInfo (..)
    , AuthEntries (..)
    -- , addOp
    , emptyAuth
    , GlobalEnvironment (..)
    , ServerEnvironment (..)
    , PluginEnvironment (..)
    , Plugin (..)
    , InitializedPlugin (..)
    , PluginState (..)
    , initializePlugin
    , runPlugin
    , runIrc
    , logMsg
    -- * Reexports
    , Text
    , Map
    , module Data.Monoid
    , module Control.Monad.Reader
    , module Control.Concurrent.STM
    , module MonadBot.Message.Encode
    ) where


import           Conduit
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State hiding (state)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import           MonadBot.Logging

import           MonadBot.Message
import           MonadBot.Message.Encode
import MonadBot.Config

class HasServerEnv s where
    getServerEnv :: (Monad m) => IrcT s m ServerEnvironment

instance HasServerEnv (PluginEnvironment a) where
    getServerEnv = asks serverEnv

instance HasServerEnv ServerEnvironment where
    getServerEnv = ask

class HasGlobalEnv s where
    getGlobalEnv :: (Monad m) => IrcT s m GlobalEnvironment

instance HasGlobalEnv GlobalEnvironment where
    getGlobalEnv = ask

instance HasGlobalEnv ServerEnvironment where
    getGlobalEnv = asks globalEnv

instance HasServerEnv s => HasGlobalEnv s where
    getGlobalEnv = globalEnv <$> getServerEnv

-- | The global environment. This contains all resources that are shared between servers.
-- The data that can be modified is stored in 'TMVars' such that any modifications will
-- be visible everywhere.
data GlobalEnvironment
    = GlobalEnvironment
    { myNick      :: Text
    , myServers   :: [ServerInfo]
    , logger      :: Logger
    -- , threadStore :: TMVar ThreadStore
    , threadLife  :: TMVar Int
    }

-- | A server environment. Contains state that is accessible in the context of a specific
-- server, plus the global state.
data ServerEnvironment
    = ServerEnvironment
    { globalEnv :: GlobalEnvironment
    , server    :: ServerInfo
    , writer    :: Writer
    }

-- | A plugin environment. Contains state that is accessible in the context of a specific
-- rnning plugin, plus the server state and global state.
data PluginEnvironment a
    = PluginEnvironment
    { serverEnv :: ServerEnvironment
    , message   :: Message
    , handler   :: InitializedPlugin a
    , state     :: PluginState a
    }

-- | Monad for irc computations.
newtype IrcT s m a
    = IrcT
    { unIrc :: ReaderT s m a
    } deriving
        ( MonadThrow, Alternative, Applicative, Monad, MonadReader s
        , MonadIO, Functor)

data Plugin s
   = Plugin
   { pluginName  :: Text
   , handlers    :: [PluginM s ()]
   , constructor :: Irc s
   , destructor  :: s -> Irc ()
   }
   -- | forall a. PersistentPlugin
   -- { construct :: Irc a -- , :: a -> Irc ()
   -- , daemons :: [PersistentPluginM a ()]
   -- }

data InitializedPlugin s
    = InitializedPlugin
    { _pluginName :: Text
    , _state      :: PluginState s
    , _handlers   :: [PluginM s ()]
    , _destructor  :: s -> Irc ()
    }

type Group = Text
type User  = Prefix

data AuthInfo
    = AuthInfo (TMVar AuthEntries)

data AuthEntries
    = AuthEntries
    { groups :: Set Group
    , users :: Map User (Set Group)
    } deriving (Show, Read, Eq)

emptyAuth :: AuthEntries
emptyAuth = AuthEntries S.empty M.empty


-- addOp :: Text -> Text -> Text -> Group -> ServerInfo -> ServerInfo
-- addOp n u h g si =
--     let (AuthEntries gs um) = authEntries si
--         new = AuthEntries
--               (S.insert g gs)
--               (M.insertWith S.union (UserPrefix n (Just u) (Just h)) (S.singleton g) um)
--     in si { authEntries = new }

data Hide f
    = forall a. Enabled (f a)
    | forall a. Disabled (f a)

data PluginState a = PluginState (TMVar a)

type Irc                 = IrcT GlobalEnvironment IO
type ServerM             = IrcT ServerEnvironment IO
type PersistentPluginM a = IrcT ServerEnvironment (StateT a IO)
type PluginM s           = IrcT (PluginEnvironment s) IO

type SimpleHandler       = PluginM () ()

type Writer              = TQueue Message

runIrc :: Irc a -> GlobalEnvironment -> IO a
runIrc irc = runReaderT (unIrc irc)

------------------------------------------------------------------
-- Plugin stuff
------------------------------------------------------------------
mkPluginState :: a -> IO (PluginState a)
mkPluginState a = do
    var <- newTMVarIO a
    return $ PluginState var

initializePlugin :: GlobalEnvironment -> Hide Plugin -> IO (Hide InitializedPlugin)
-- initializePlugin _ (PersistentPlugin {..}) =
--     error "initializePlugin (PersistentPlugin (..)): not defined"
initializePlugin env (Enabled (Plugin {..})) = do
    x <- runIrc constructor env
    s <- mkPluginState x
    return $ Enabled $ InitializedPlugin pluginName s handlers destructor

runPlugin :: MonadIO m => PluginEnvironment s -> PluginM s a -> m a
runPlugin pEnv h =  liftIO $ runReaderT (unIrc h) pEnv

------------------------------------------------------------------
-- Utility
------------------------------------------------------------------

logMsg :: (MonadIO m, HasGlobalEnv s) => Text -> IrcT s m ()
logMsg msg = do
    lgr <- logger `fmap` getGlobalEnv
    liftIO . atomically . writeTQueue lgr $ msg
