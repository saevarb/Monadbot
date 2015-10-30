{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Auth
    ( plugin
    ) where

import           Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import           MonadBot.Plugin.Development


data AuthState = AuthState (Maybe (Text, Text))

addGroup :: PluginM a ()
addGroup =
    handleBang "!addgroup" $
    whenInGroup "owner" $ do
    (chan:_:g:_) <- getParams
    void $ modifyAuthEntries $ \ae ->
        ae { groups = nub (g:groups ae) }
    sendPrivmsg chan ["Group " <> g <> " added."]

addUser :: PluginM AuthState ()
addUser =
    handleBang "!adduser" $
    whenInGroup "owner" $ do
    (AuthEntries gs _) <- getAuthEntries
    (chan:_:u:g:_) <- getParams
    if g `elem` gs
    then do
        void $ swapState (AuthState (Just (g, chan)))
        sendCommand "WHOIS" [u]
    else sendPrivmsg chan ["Group " <> g <> " does not exist."]

handleWhois :: PluginM AuthState ()
handleWhois =
    handles "311" $ do
    (AuthState (Just (g, chan))) <- readState
    (_:n:u:h:_) <- getParams
    sendPrivmsg chan ["Added user " <> n <> "."]
    (AuthEntries gs _) <- getAuthEntries
    void $ modifyAuthEntries $ \ae ->
        ae { users = M.insert (UserPrefix n (Just u) (Just h)) g (users ae)}
    return ()

listGroups :: PluginM a ()
listGroups =
    handleBang "!listgroups" $
    whenInGroup "owner" $ do
    (AuthEntries gs _) <- getAuthEntries
    (chan:_) <- getParams
    sendPrivmsg chan ["My user groups are: " <> T.intercalate ", " gs]

listUsers :: PluginM a ()
listUsers =
    handleBang "!listusers" $
    whenInGroup "owner" $ do
    (AuthEntries _ um) <- getAuthEntries
    (chan:_) <- getParams
    sendPrivmsg chan ["These are my operators:"]
    sendPrivmsg chan ["Group - Users"]
    forM_ (groupBy (\a b -> snd a == snd b) . sortOn snd $ M.toList um) $ \gr -> do
        let g = snd . head $ gr
        sendPrivmsg chan [g <> " - " <> T.intercalate "," (map (pNick . fst) gr)]

plugin :: Plugin AuthState
plugin =
    Plugin
    "User and group management plugin"
    [addGroup, addUser, listGroups, listUsers, handleWhois]
    (return (AuthState Nothing))
    (const $ return ())
