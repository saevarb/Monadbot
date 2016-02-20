{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module MonadBot.Plugins.Test
    ( plugin
    ) where

import qualified Database.Persist as DB
import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import           System.Random

import MonadBot.Plugin.Development

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
   name Text
   value Int
   deriving Show
|]


serverCmd :: SimpleHandler
serverCmd =
    onUserCmd "$servers" $ do
        srvs <- myServers <$> getGlobalEnv
        (chan:_) <- getParams
        sendCommand "PRIVMSG" [chan , "I am on the following servers and channels:"]
        forM_ srvs $ \srv ->
            sendCommand "PRIVMSG" [chan, serverAddress srv <> ": " <> T.intercalate ", " (serverChannels srv)]

stateTest = do
    onUserCmd "$write" $ do
        (chan:_:n:v:_) <- getParams
        sendPrivmsg chan ["Writing entry:", n <> ":" <> v, "to database"]
        runDb $ DB.insert $ Foo n (read $ T.unpack v)
        return ()
    onUserCmd "$findv" $ do
        (chan:_:v:_) <- getParams
        res <- runDb $ DB.selectList [FooValue DB.==. (read $ T.unpack v)] []
        case res of
          [] -> sendPrivmsg chan ["No values found."]
          vs -> sendPrivmsg chan ["Found values:", T.concat $ map (T.pack . show . entityVal) res]


foo :: Irc ()
foo = initializeDb (runMigration migrateAll)

plugin :: Plugin ()
plugin = Plugin "Test handler" [stateTest] foo (const $ return ())
