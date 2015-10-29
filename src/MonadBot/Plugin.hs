module MonadBot.Plugin
    ( allPlugins
    ) where
import MonadBot.Types

import qualified MonadBot.Plugin.Ping as Ping
import qualified MonadBot.Plugin.Join as Join
import qualified MonadBot.Plugin.Test as Test
import qualified MonadBot.Plugin.Darchoods as Darchoods
import qualified MonadBot.Plugin.Version as Version
import qualified MonadBot.Plugin.Nixers as Nixers

allPlugins :: [Hide Plugin]
allPlugins =
    [ Hide Ping.plugin
    , Hide Join.plugin
    , Hide Test.plugin
    , Hide Darchoods.plugin
    , Hide Version.plugin
    , Hide Nixers.plugin
    ]
