{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Auth
    ( plugin
    ) where

import           Data.List
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           MonadBot.Plugin.Development


data AuthState = AuthState (Maybe (Text, Text))

addGroup :: PluginM a ()
addGroup =
    onUserCmd "$addgroup" $
    whenInGroup "owner" $ do
    (chan:_:g:_) <- getParams
    void $ modifyAuthEntries $ \ae ->
        ae { groups = S.insert g (groups ae) }
    sendPrivmsg chan ["Group " <> g <> " added."]

addUser :: PluginM AuthState ()
addUser =
    onUserCmd "$adduser" $
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
    onCmd "311" $ do
    (AuthState (Just (g, chan))) <- readState
    (_:n:u:h:_) <- getParams
    sendPrivmsg chan ["Added user " <> n <> "."]
    (AuthEntries _ um) <- getAuthEntries
    void $ modifyAuthEntries $ \ae ->
        ae { users = M.insertWith S.union (UserPrefix n (Just u) (Just h)) (S.singleton g) um}
    return ()

listGroups :: PluginM a ()
listGroups =
    onUserCmd "$listgroups" $
    whenInGroup "owner" $ do
    (AuthEntries gs _) <- getAuthEntries
    (chan:_) <- getParams
    sendPrivmsg chan ["My user groups are: " <> T.intercalate ", " (S.toList gs)]

listUsers :: PluginM a ()
listUsers =
    onUserCmd "$listusers" $
    whenInGroup "owner" $ do
    (AuthEntries _ um) <- getAuthEntries
    (chan:_) <- getParams
    sendPrivmsg chan ["These are my operators:"]
    forM_ (M.toList um) $ \(u, gs) ->
        sendPrivmsg chan [pNick u <> " - {" <> T.intercalate ", " (S.toList gs) <> "}"]

plugin :: Plugin AuthState
plugin =
    Plugin
    "User and group management plugin"
    [addGroup, addUser, listGroups, listUsers, handleWhois]
    (return (AuthState Nothing))
    (const $ return ())
