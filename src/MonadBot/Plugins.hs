module MonadBot.Plugins
    ( allPlugins
    ) where
import MonadBot.Types

import qualified MonadBot.Plugins.Ping as Ping
import qualified MonadBot.Plugins.Join as Join
import qualified MonadBot.Plugins.Test as Test
import qualified MonadBot.Plugins.Darchoods as Darchoods
import qualified MonadBot.Plugins.Version as Version

allPlugins :: [Hide Plugin]
allPlugins =
    [ Hide Ping.plugin
    , Hide Join.plugin
    , Hide Test.plugin
    , Hide Darchoods.plugin
    , Hide Version.plugin
    ]
